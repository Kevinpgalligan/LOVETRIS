;;;; AI to place the pieces.

(in-package lovetris)

(defun run-searcher (searcher-init &key max-states log)
  "Returns final score and hex-encoded sequence of moves."
  (let* ((i 0)
         (state (make-state))
         (searcher (funcall searcher-init state)))
    (let ((states
            (loop while (and (not (game-over state))
                             (or (not max-states)
                                 (< i max-states)))
                  do (incf i)
                  do (when log
                       (format t "State #~a, score ~a~%" i (score state)))
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
  '(("LL" . "0")
    ("LR" . "1")
    ("LD" . "2")
    ("LU" . "3")
    ("RL" . "4")
    ("RR" . "5")
    ("RD" . "6")
    ("RU" . "7")
    ("DL" . "8")
    ("DR" . "9")
    ("DD" . "A")
    ("DU" . "B")
    ("UL" . "C")
    ("UR" . "D")
    ("UD" . "E")
    ("UU" . "F")))

(defun move-pair-to-hex (move-pair)
  (cdr (assoc move-pair *hex-mapping* :test #'equalp)))

(defun decode-game (encoding)
  (apply #'concatenate
         (cons 'string
               (loop for c across encoding
                     collect (car (rassoc (string c) *hex-mapping* :test #'equalp))))))

(defclass node ()
  ((parents
    :initarg :parents
    :initform nil
    :accessor parents)
   (state
    :initarg :state
    :initform (error "Must provide state for node.")
    :reader state)
   (heuristic-value
    :initarg :heuristic-value
    :initform nil
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

(defun count-nodes (node)
  "Count nodes in the tree rooted at the given node."
  (+ 1 (apply #'+ (mapcar #'count-nodes (children node)))))

(defun destroy-tree (node keep-cache)
  "Attempts to break references between nodes in the given tree, as a hint
to the garbage collector that it can collect."
  (when (not (get-node keep-cache node))
    (loop for child in (children node)
          do (destroy-tree child keep-cache))
    (setf (children node) nil)))

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
  (setf (gethash node (hashset cache)) node))

(defgeneric get-node (cache node)
  (:documentation "Get this node from the cache, if it (or an equivalent
node object) exists already."))
(defmethod get-node ((cache node-cache) node)
  (gethash node (hashset cache)))

(defun node-hash (node)
  (state-hash (state node)))

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
              ;; This seems to be a weird definition of
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
