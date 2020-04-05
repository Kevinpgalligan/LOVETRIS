(in-package lovetris)

(defconstant +empty+ 0)
(defconstant +full+ 1)

(defstruct state well score)

(defun new-state (&key (width 10) (height 16))
  (make-state :well (make-array (list height width) :initial-contents +empty+)
              :score 0))

(defun get-cell (state x y)
  (aref (state-well game) y x))

;; things to do:
;;   1. generate next possible moves given a certain piece.
;;      1.1. define pieces & their unique rotations.
;;      1.2. algorithm to find valid positions (don't copy whole
;;           grid yet, have some sort of "intermediate" copy that's
;;           just a pair of the well and the piece position... maybe.
;;           Think about what will be useful later on).
;;   2. Hatetris AI: looks at all possible moves for all possible
;;      pieces, picks the piece with the max minimum height? Tie breaks
;;      solved by order of pieces. Need to read the code.
;;   3. Some sort of interface for testing it and playing with it.
;;   4. Greedy search (algorithm and heuristics).
;;   5. Rework genetic algo code to find best heuristic weights.
;;   6. Implement Monte Carlo Tree Search.
