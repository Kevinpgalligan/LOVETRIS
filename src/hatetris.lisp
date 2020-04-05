(in-package lovetris)

(defconstant +empty+ 0)
(defconstant +full+ 1)

(defstruct state well score)

(defun new-state (&key (width 10) (height 16))
  (make-state :well (make-array (list height width) :initial-contents +empty+)
              :score 0))

(defun get-cell (state x y)
  (aref (state-well game) y x))

;; Pieces have a 4x4 bounding box, x & y point
;; to the top left corner of the bounding box.
;; They also store the ID of the piece. Pointers
;; to the parts of the bounding box that are filled
;; in. And the orientation of the piece. Orientation
;; is changed in conjunction with the pointers to the
;; filled in parts.
(defstruct piece x y id filled-coords orientation)

(defun piece-down (piece)
  (make-piece :x (piece-x piece)
              :y (1+ (piece-y piece))
              :id (piece-id piece)
              :filled-coords (piece-filled-coords piece)
              :orientation (piece-orientation piece)))

(defun piece-left (piece)
  (make-piece :x (1- (piece-x piece))
              :y (piece-y piece)
              :id (piece-id piece)
              :filled-coords (piece-filled-coords piece)
              :orientation (piece-orientation piece)))

(defun piece-right (piece)
  (make-piece :x (1+ (piece-x piece))
              :y (piece-y piece)
              :id (piece-id piece)
              :filled-coords (piece-filled-coords piece)
              :orientation (piece-orientation piece)))

(defun piece-rotate (piece)
  (multiple-value-bind (new-filled-coords new-orientation)
      (next-orientation (piece-id piece) (piece-orientation))
    (make-piece :x (piece-x piece)
                :y (piece-y piece)
                :id (piece-id piece)
                :filled-coords new-filled-coords
                :orientation new-orientation)))

(defconstant piece-bbox-size 4)

(defparameter piece-orientations
  (make-hash-table :test #'equal))

(defun get-orientations (piece-id)
  (gethash piece-id piece-orientations))

(defun next-orientation (piece-id orientation)
  (let* ((orientations (get-orientations piece-id))
         (next-orientation-index (mod (1+ orientation)
                                      (length orientations))))
    (values next-orientation-index
            (nth next-orientation-index orientations))))

(defun add-piece (template)
  ;; Create & return constructor function, and
  ;; also add the piece's orientations to the list.
  (when (not (= +piece-bbox-size+ (length template)))
    (error "Piece is the wrong size." template))
  (defparameter filled-coords (list))
  (loop for y from 0 below +piece-bbox-size+ do
        (let ((row (nth y template)))
          (when (not (= +piece-bbox-size+ (length row)))
            (error "Piece row is wrong size." template))
          (loop for x from 0 below +piece-bbox-size+ do
                (when (equal #\# (char row x))
                  (push (list x y) (car filled-coords))))))
  ;; TODO do rotations (x=4-y-1, y=x), add to orientations list.
  ;; then return a constructor.
  ;; Also... should moving a piece always create a new struct?
  ;; Seems a bit wasteful.
  ;; Not sure what the next steps are after that.
  )

;; Order is important, as described in
;; the original HATETRIS code.
(defconstant +piece-constructors+
  (mapcar
   #'add-piece
   ;; Templates.
   (list
    '("...."
      "..##"
      ".##."
      "....")
    '("...."
      ".##."
      "..##"
      "....")
    '("...."
      ".##."
      ".##."
      "....")
    '("...."
      "####"
      "...."
      "....")
    '("...."
      ".###"
      ".#.."
      "....")
    '("...."
      ".##."
      ".#.."
      ".#..")
    '("...."
      ".###"
      "..#."
      "....")
    )))
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
