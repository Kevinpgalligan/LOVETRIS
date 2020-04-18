(in-package lovetris)

(defclass beam-searcher ()
  ((beam-width
    :initarg :beam-width
    :initform 2
    :reader beam-width
    :documentation "How many states to explore at each depth of the search tree.")
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
                searcher)
  (let ((next-node
          (alexandria:extremum (children (search-tree searcher))
                               #'>
                               :key #'heuristic-value)))
    (setf (search-tree searcher) next-node)
    (state next-node)))

;; We use this to find the path through the search tree that
;; has the best heuristic score at the end of it.
(defun expand-nodes (current-node remaining-depth searcher)
  (setf (heuristic-value current-node)
        (if (= 0 remaining-depth)
            ;; We've reached the bottom of the search tree, resort
            ;; to direct heuristic evaluation.
            (funcall (heuristic-eval searcher) (state current-node))
            ;; Haven't reached the bottom yet, expand further.
            (progn
              (when (not (children current-node))
                (generate-children! current-node searcher))
              ;; Might be a dead end (i.e. game over), so just use
              ;; heuristic value.
              (if (children current-node)
                  (apply #'max
                         (mapcar (lambda (child)
                                   (expand-nodes child (1- remaining-depth) searcher))
                                 (children current-node)))
                  (heuristic-value current-node))))))

(defun generate-children! (node searcher)
  (setf (children node)
        (let ((possible-child-nodes
                (sort
                 (mapcar (lambda (state)
                           (make-node
                            state
                            :heuristic-value (funcall (heuristic-eval searcher)
                                                      state)))
                         (possible-next-states (state node)))
                 #'>
                 :key #'heuristic-value)))
          ;; Only add the best N nodes to the tree, where N
          ;; is the width of the beam search.
          (loop for child in possible-child-nodes
                for i from 0 below (beam-width searcher)
                collect child))))
