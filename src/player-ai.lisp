;;;; AI to place the pieces.

(in-package lovetris)

;;; Look 1 move ahead and pick the next state
;;; that maximises heuristics.
(defun place-greedily (heuristic-evaluator)
  (lambda (state piece)
    (let* ((state-score-pairs
             (mapcar (lambda (next-state)
                       (cons next-state
                             (funcall heuristic-evaluator
                                      next-state)))
                     (get-placements state piece)))
           (best-score (apply #'max (mapcar #'cdr state-score-pairs)))
           (best-states
             (remove-if-not (lambda (pair)
                              (= best-score (cdr pair)))
                            state-score-pairs)))
      ;; If multiple states have the best score, pick one
      ;; at random.
      (car (nth (random (length best-states))
                best-states)))))

(defclass beam-searcher ()
  ((beam-width
    :initarg :beam-width
    :initform 2
    :reader beam-width
    :documentation "How many states to explore at each depth of the search tree.")
   (search-depth
    :initarg :search-depth
    :initform 16
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

(defclass node ()
  ((state
    :initarg :state
    :initform (error "Must provide state for node.")
    :reader state)
   (heuristic-value
    :initarg :heuristic-value
    :accessor heuristic-value)
   (children
    :initarg :children
    :initform nil
    :accessor children)))

(defun make-node (state &key heuristic-value)
  (make-instance 'node
                 :state state
                 :heuristic-value heuristic-value))

(defgeneric advance (tree-searcher)
  (:documentation "Get tree searcher to advance and return the next state."))

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

;;; Returns a heuristic function with the given
;;; weights on different characteristics of the
;;; game.
(defun get-heuristic-eval (w-aggregate-height w-score w-holes w-bumpiness)
  (lambda (state)
    (+ (* w-aggregate-height (aggregate-height state))
       (* w-score (state-score state))
       (* w-holes (num-holes state))
       (* w-bumpiness (bumpiness state)))))

(defun aggregate-height (state)
  (loop for x from 0 below (well-width state)
        sum (column-height state x)))

(defun column-height (state x)
  (loop for y from 0 below (well-height state)
        while (equalp +empty+ (get-square state x y))
        finally (return (- (well-height state) y))))

(defun num-holes (state)
  (loop for x from 0 below (well-width state) sum
        (loop for y from 1 below (well-height state) sum
              ;; Should probably abstract the square types
              ;; better. Have functions like is-empty-square-p
              ;; and is-full-square-p.
              ;; Also, this seems to be a weird definition of
              ;; a "hole", should it perhaps be surrounded on
              ;; the sides as well?
              (if (and (equalp +empty+ (get-square state x y))
                       (equalp +full+ (get-square state x (- y 1))))
                  1
                  0))))

(defun bumpiness (state)
  (loop for x from 1 below (well-width state) sum
        ;; Ugh. Computing height of each column twice.
        (abs (- (column-height state x)
                (column-height state (- x 1))))))
