(in-package lovetris)

(defclass beam-searcher ()
  ((beam-width
    :initarg :beam-width
    :initform 2
    :reader beam-width
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
    :accessor search-tree)))

(defmethod initialize-instance :after ((searcher beam-searcher)
                                       &key start-state)
  (when (not start-state)
    (error "No state to start from."))
  (setf (search-tree searcher) (make-node start-state)))

(defmethod advance ((searcher beam-searcher))
  (expand-nodes (search-tree searcher)
                (search-depth searcher)
                (heuristic-eval searcher)
                (if (disable-beam searcher)
                    nil
                    (beam-width searcher)))
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

;; We use this to find the path through the search tree that
;; has the best heuristic score at the end of it.
(defun expand-nodes (current-node remaining-depth heuristic-eval beam-width)
  (setf (heuristic-value current-node)
        (if (= 0 remaining-depth)
            ;; We've reached the bottom of the search tree, resort
            ;; to direct heuristic evaluation.
            (funcall heuristic-eval (state current-node))
            ;; Haven't reached the bottom yet, expand further.
            (progn
              (when (not (children current-node))
                (generate-children! current-node heuristic-eval beam-width))
              ;; Might be a dead end (i.e. game over), so just use
              ;; heuristic value.
              (if (children current-node)
                  (apply #'max
                         (mapcar (lambda (child)
                                   (expand-nodes child
                                                 (1- remaining-depth)
                                                 heuristic-eval
                                                 beam-width))
                                 (children current-node)))
                  (heuristic-value current-node))))))

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
