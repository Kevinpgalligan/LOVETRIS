;;;; AI to place the pieces.

(in-package lovetris)

(defun run-searcher (searcher-init &key max-states)
  "Returns final score and hex-encoded sequence of moves."
  (let* ((i 0)
         (state (make-state))
         (searcher (funcall searcher-init state)))
    (let ((states
            (loop while (and (not (game-over state))
                             (or (not max-states)
                                 (< i max-states)))
                  do (incf i)
                  do (setf state (advance searcher))
                  collect state)))
      (values (score (car (last states)))
              (encode-game states)))))

(defun encode-game (states)
  (let* ((moves (extract-moves states))
         (n-moves (length moves)))
    (apply #'concatenate
           'string
           (mapcar #'move-pair-to-hex
                   (loop :for (a b)
                         :on (if (= 0 (mod n-moves 2))
                                 moves
                                 (append moves (list "D")))
                         :by #'cddr
                         :while b
                         :collect (concatenate 'string a b))))))

(defun extract-moves (states)
  (apply #'append
         (mapcar (lambda (state)
                   ;; Gotta reverse, moves are stored most recent first.
                   (reverse (last-move-sequence state)))
                 states)))

(defparameter *hex-mapping*
  '(("LL" "0")
    ("LR" "1")
    ("LD" "2")
    ("LU" "3")
    ("RL" "4")
    ("RR" "5")
    ("RD" "6")
    ("RU" "7")
    ("DL" "8")
    ("DR" "9")
    ("DD" "A")
    ("DU" "B")
    ("UL" "C")
    ("UR" "D")
    ("UD" "E")
    ("UU" "F")))

(defun move-pair-to-hex (move-pair)
  (cadr (assoc move-pair *hex-mapping* :test #'equalp)))

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
    :reader children)
   (expanded
    :initarg :expanded
    :initform nil
    :accessor expanded)))

(defun (setf children) (child-nodes node)
  (setf (slot-value node 'children) child-nodes)
  ;; If we add children to a node, also mark it as expanded.
  (setf (expanded node) t))

(defun make-node (state &key heuristic-value)
  (make-instance 'node
                 :state state
                 :heuristic-value heuristic-value))

(defun count-nodes (node)
  "Count nodes in the tree rooted at the given node."
  (+ 1 (apply #'+ (mapcar #'count-nodes (children node)))))

(defun destroy-tree (node)
  "Attempts to break references between nodes in the given tree, as a hint
to the garbage collector that it can collect."
  (mapcar #'destroy-tree (children node))
  (setf (children node) nil))

(defgeneric advance (tree-searcher)
  (:documentation "Get tree searcher to advance and return the next state."))

;;; Returns a heuristic function with the given
;;; weights on different characteristics of the
;;; game.
(defun get-heuristic-eval (w-aggregate-height w-score w-holes w-bumpiness)
  (lambda (state)
    (+ (* w-aggregate-height (aggregate-height state))
       (* w-score (score state))
       (* w-holes (num-holes state))
       (* w-bumpiness (bumpiness state)))))

(defun aggregate-height (state)
  (loop for x from 0 below (well-width state)
        sum (column-height state x)))

(defun column-height (state x)
  (loop for y from 0 below (well-height state)
        while (empty-square-p state x y)
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
              (if (and (empty-square-p state x y)
                       (full-square-p state x (- y 1)))
                  1
                  0))))

(defun bumpiness (state)
  (loop for x from 1 below (well-width state) sum
        ;; Ugh. Computing height of each column twice.
        (abs (- (column-height state x)
                (column-height state (- x 1))))))
