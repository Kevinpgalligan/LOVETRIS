(in-package lovetris)

(defclass beam-searcher ()
  ((beam-width
    :initarg :beam-width
    :initform 2
    :accessor beam-width
    :documentation "How many states to explore at each depth of the search tree.")
   (disable-beam
    :initarg :disable-beam
    :initform nil
    :reader disable-beam
    :documentation "If true, removes the beam width limitation and this turns into plain ol' greedy search.")
   (search-depth
    :initarg :search-depth
    :initform 6
    :reader search-depth
    :documentation "How deep to search in the tree before picking a move.")
   (heuristic-eval
    :initarg :heuristic-eval
    :reader heuristic-eval
    :initform (error "Must supply heuristic evaluator for beam search.")
    :documentation "A function that estimates the 'goodness' of a state, should return a real number. Higher is better.")
   (search-tree
    :accessor search-tree)
   (num-threads
    :initarg :num-threads
    :initform 1
    :reader num-threads
    :documentation "How many threads to use for expanding the search tree.")))

(defmethod initialize-instance :after ((searcher beam-searcher)
                                       &key start-state)
  (when (not start-state)
    (error "No state to start from."))
  (setf (search-tree searcher) (make-node start-state))
  (when (disable-beam searcher)
    (setf (beam-width searcher) nil)))

(defmethod advance ((searcher beam-searcher))
  (expand-nodes! (list (search-tree searcher))
                 (search-depth searcher)
                 (heuristic-eval searcher)
                 (beam-width searcher)
                 (num-threads searcher))
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

(defun expand-nodes! (nodes remaining-depth heuristic-eval beam-width num-threads)
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
                             beam-width
                             num-threads))
      (when leftover-nodes
        (loop for node in leftover-nodes do
              (when (not (expanded node))
                (generate-children! node heuristic-eval beam-width)))
        (expand-nodes! (apply #'append
                              (mapcar #'children
                                      leftover-nodes))
                       (1- remaining-depth)
                       heuristic-eval
                       beam-width
                       num-threads)))))

(defun expand-with-threads (nodes remaining-depth heuristic-eval beam-width num-threads)
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
                                              beam-width)))))))))
      ;; Now wait for the threads to terminate.
      (loop for thread in threads do
            (bt:join-thread thread)))))

(defun generate-children! (node heuristic-eval beam-width)
  (setf (children node)
        (let ((possible-child-nodes
                (sort
                 (mapcar (lambda (state)
                           (make-node
                            state
                            :heuristic-value (funcall heuristic-eval state)))
                         (possible-next-states (state node)))
                 #'>
                 :key #'heuristic-value)))
          (if (null beam-width)
              possible-child-nodes
              ;; Only add the best N nodes to the tree, where N
              ;; is the width of the beam search.
              (loop for child in possible-child-nodes
                    for i from 0 below beam-width
                    collect child)))))

(defun add-leaves! (node remaining-depth heuristic-eval beam-width)
  (when (< 0 remaining-depth)
    (when (not (expanded node))
      (generate-children! node heuristic-eval beam-width))
    (loop for child in (children node) do
          (add-leaves! child (1- remaining-depth) heuristic-eval beam-width))))

(defun propagate-heuristic-values! (node)
  (if (not (children node))
      (heuristic-value node)
      (setf (heuristic-value node)
            (apply #'max
                   (mapcar #'propagate-heuristic-values!
                           (children node))))))
