;;;; AI to place the pieces.

(in-package lovetris)

(defun run-searcher (searcher-init &key max-states initial-state process-fn)
  "Returns final score, a hex-encoded sequence of moves, and the final state.
SEARCHER-INIT is a function that accepts the initial hatetris
state and returns a searcher (which implements the ADVANCE
generic function).
PROCESS-FN is called on each new state and move sequence."
  (let* ((state (or initial-state (make-state)))
         (searcher (funcall searcher-init state)))
    (let ((move-sequences
            (loop for i = 0 then (1+ i)
                  while (and (not (game-over state))
                             (or (not max-states)
                                 (< i max-states)))
                  collect (multiple-value-bind (next-state move-sequence)
                              (advance searcher)
                            (setf state next-state)
                            (when process-fn
                              (funcall process-fn next-state move-sequence))
                            move-sequence))))
      (values (score state)
              (encode-game (apply #'append move-sequences))
              state))))

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
   (edges
    :initarg :edges
    :initform nil
    :reader edges
    :documentation "Edges are pointers to child nodes paired with
move sequences that are necessary to transition to that child node.")
   (expanded
    :initarg :expanded
    :initform nil
    :accessor expanded)))

(defgeneric children (node))
(defmethod children ((node node))
  (mapcar #'edge-child (edges node)))

(defun (setf edges) (edges node)
  (setf (slot-value node 'edges) edges)
  ;; If we add children to a node, also mark it as expanded.
  (setf (expanded node) t))

(defun make-edge (move-sequence child)
  ;; For now, just go with a pair.
  (cons move-sequence child))

(defun edge-child (edge)
  (cdr edge))

(defun edge-move-sequence (edge)
  (car edge))

(defun count-nodes (node)
  "Count nodes in the tree rooted at the given node."
  (+ 1 (apply #'+ (mapcar #'count-nodes (children node)))))

(defun destroy-tree (node keep-cache)
  "Attempts to break references between nodes in the given tree, as a hint
to the garbage collector that it can collect."
  (when (not (get-node keep-cache node))
    (loop for child in (children node)
          do (destroy-tree child keep-cache))
    (setf (edges node) nil)))

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

(defun make-node-cache (node)
  (let ((cache (make-instance 'node-cache)))
    (populate-cache cache node)
    cache))

(defun populate-cache (cache node)
  (add cache node)
  (loop for child in (children node) do
        (populate-cache cache child)))

(defun node-hash (node)
  (state-hash (state node)))

(defgeneric advance (tree-searcher)
  (:documentation "Get tree searcher to advance and return the next state + move sequence."))

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
