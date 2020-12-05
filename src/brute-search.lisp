(in-package lovetris)

(defclass brute-searcher ()
  ((search-depth
    :initarg :search-depth
    :initform 6
    :reader search-depth
    :documentation "How deep to search in the tree before picking a move.")
   (heuristic-eval
    :initarg :heuristic-eval
    :reader heuristic-eval
    :initform (error "Must supply heuristic evaluator.")
    :documentation "A function that estimates the 'goodness' of a state, should return a real number. Higher is better.")
   (search-tree
    :accessor search-tree)
   (num-threads
    :initarg :num-threads
    :initform 1
    :reader num-threads
    :documentation "How many threads to use for expanding the search tree.")))

(defmethod initialize-instance :after ((searcher brute-searcher)
                                       &key start-state)
  (when (not start-state)
    (error "No state to start from."))
  (setf (search-tree searcher) (make-node start-state)))

(defmethod advance ((searcher brute-searcher))
  (expand-nodes! (list (search-tree searcher))
                 (search-depth searcher)
                 (heuristic-eval searcher)
                 (num-threads searcher)
                 (create-node-cache (search-tree searcher)))
  (propagate-heuristic-values! (search-tree searcher))
  (let ((next-node
          (alexandria:extremum (children (search-tree searcher))
                               #'>
                               :key #'heuristic-value)))
    (loop for node in (children (search-tree searcher)) do
          ;; This should help SBCL to perform garbage collection
          ;; properly. In large data structures, such as trees, SBCL's
          ;; garbage collector can leave entire branches of the tree
          ;; uncollected after we've discarded them, due to its conservatism.
          ;; Eventually, this will cause heap exhaustion. A rather nasty
          ;; trait of the implementation.
          (when (not (eq node next-node))
            (destroy-tree node)))
    (setf (search-tree searcher) next-node)
    (state next-node)))

(defun create-node-cache (node)
  (let ((cache (make-instance 'node-cache)))
    (populate-cache cache node)
    cache))

(defun populate-cache (cache node)
  (add cache node)
  (loop for child in (children node) do
        (populate-cache cache child)))

(defun expand-nodes! (nodes remaining-depth heuristic-eval num-threads node-cache)
  (when (> remaining-depth 0)
    ;; Divide nodes as evenly as possible among threads for expansion, we
    ;; continue the single-threaded expansion of any remaining nodes.
    (let* ((split-index (* num-threads (floor (length nodes) num-threads)))
           (nodes-for-threads (subseq nodes 0 split-index))
           (leftover-nodes (subseq nodes split-index)))
      (when nodes-for-threads
        (expand-with-threads nodes-for-threads
                             remaining-depth
                             heuristic-eval
                             num-threads
                             node-cache))
      (when leftover-nodes
        (loop for node in leftover-nodes do
              (when (not (expanded node))
                (generate-children! node heuristic-eval node-cache)))
        (expand-nodes! (apply #'append
                              (mapcar #'children
                                      leftover-nodes))
                       (1- remaining-depth)
                       heuristic-eval
                       num-threads
                       node-cache)))))

(defun expand-with-threads (nodes remaining-depth heuristic-eval num-threads node-cache)
  (let ((nodes-lock (bt:make-lock)))
    (let ((threads
            (loop for i below num-threads collect
                  (bt:make-thread
                   (lambda ()
                     (loop (let ((node
                                   (bt:with-lock-held (nodes-lock)
                                     (pop nodes))))
                             (if (null node)
                                 (return)
                                 (add-leaves! node
                                              remaining-depth
                                              heuristic-eval
                                              node-cache)))))))))
      ;; Now wait for the threads to terminate.
      (loop for thread in threads do
            (bt:join-thread thread)))))

(defun generate-children! (node heuristic-eval node-cache)
  (let ((child-nodes
          (remove-if (lambda (node)
                       (contains node-cache node))
                     (mapcar (lambda (state)
                               (make-node
                                state
                                :heuristic-value (funcall heuristic-eval state)))
                             (possible-next-states (state node))))))
    (setf (children node) child-nodes)
    ;; There is a race condition here. Threads A & B both
    ;; check the cache for node N, it's not there. Then they
    ;; both add it to their respective branches of the tree, and
    ;; to the node cache. But we're accepting that risk in exchange
    ;; for simplicity of implementation. It's not the end of the world
    ;; if we explore a branch of the search tree needlessly due to
    ;; a failure of the cache.
    (loop for child in (children node) do
          (add node-cache child))))

(defun add-leaves! (node remaining-depth heuristic-eval node-cache)
  (when (< 0 remaining-depth)
    (when (not (expanded node))
      (generate-children! node heuristic-eval node-cache))
    (loop for child in (children node) do
          (add-leaves! child (1- remaining-depth) heuristic-eval node-cache))))

(defun propagate-heuristic-values! (node)
  (if (not (children node))
      (heuristic-value node)
      (setf (heuristic-value node)
            (apply #'max
                   (mapcar #'propagate-heuristic-values!
                           (children node))))))

(defclass node-cache ()
  ((hashset
    :initform (make-hash-table :hash-function #'node-hash
                               :test #'nodes-equivalent-p
                               ;; Will be accessed by multiple threads, so...
                               :synchronized t)
    :reader hashset)))

(defun nodes-equivalent-p (n1 n2)
  (states-equivalent-p (state n1) (state n2)))

(defgeneric add (cache node)
  (:documentation "Add node to cache."))
(defmethod add ((cache node-cache) node)
  (setf (gethash node (hashset cache)) t))

(defgeneric contains (cache node)
  (:documentation "Whether cache contains the given node."))
(defmethod contains ((cache node-cache) node)
  (gethash node (hashset cache)))

(defun node-hash (node)
  (state-hash (state node)))
