;;;; AI to place the pieces.

(in-package lovetris)

(defun place-randomly (state piece)
  (let ((options (get-placements state piece)))
    (nth (random (length options))
         options)))

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

#|
(defclass beam-searcher ()
  ((beam-width
    :initarg :beam-width
    :initform 2
    :documentation "How many states to explore at each depth of the search tree.")
   (search-depth
    :initarg :search-depth
    :initform 16
    :accessor search-depth
    :documentation "How deep to search in the tree before picking a move.")
   (search-tree
    :initarg :search-tree
    :initform nil)))

(defgeneric init-search-tree (tree-searcher start-state))

(defmethod init-search-tree ((tree-searcher beam-searcher) start-state)
  (setf (slot-value beam-searcher 'search-tree)
        (make-node start-state)))

(defclass node ()
  ((state
    :initform (error "Must provide state for node.")
    :reader state)
   (heuristic-value
    :initarg :heuristic-value
    :reader heuristic-value)
   (children
    :initform nil
    :accessor children)))

(defun make-node (state)
  (make-instance 'node
                 :state state))

(defun expanded-p (node)
  (children node))

(defgeneric advance (tree-searcher)
  (:documentation "Get tree searcher to advance and return the next state."))

(defmethod advance ((searcher beam-searcher))
  (expand-nodes
   (search-tree searcher)
   (search-width searcher)
   (search-depth searcher))
  (let ((next-node (best-node (children (search-tree searcher)))))
    (setf (search-tree searcher) next-node)
    (state next-node)))

;; TODO implement this.
;; Needs heuristic evaluator.
;; If at the bottom... set heuristic
;; value of this node to its own value. And return that.
;; If it hasn't been expanded, add child nodes.
;; Then set its heuristic value to the max of the heuristic
;; values of its children. Oh shit, wait. Gotta rank the
;; children by their heuristic values and only pick the highest
;; ones ('width' of them).
(defun expand-nodes (current-node width remaining-depth)
  (cond ((= 0 remaining-depth)
         (setf (heuristic-value)))))

(defun best-node (nodes)
  (alexandria:extremum nodes #'> :key #'heuristic-value))

|#

;;; Returns a heuristic function with the given
;;; weights on different characteristics of the
;;; game.
(defun heuristic-evaluator (w-aggregate-height w-score w-holes w-bumpiness)
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
