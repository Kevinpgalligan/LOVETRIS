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
