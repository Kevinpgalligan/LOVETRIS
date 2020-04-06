(in-package lovetris)

(defconstant +empty+ 0)
(defconstant +full+ 1)

(defstruct state well score)

(defun new-state (&key (width 10) (height 20))
  (make-state :well (make-array (list height width) :initial-element +empty+)
              :score 0))

(defun get-cell (state x y)
  (aref (state-well state) y x))

;; Pieces have a 4x4 bounding box, x & y point
;; to the top left corner of the bounding box.
;; They also store the ID of the piece. Pointers
;; to the parts of the bounding box that are filled
;; in. And the orientation of the piece. Orientation
;; is changed in conjunction with the pointers to the
;; filled in parts.
(defstruct piece x y id coords orientation)

(defun adjusted-piece (piece overrides)
  (let ((copy (copy-structure piece)))
    (loop for (slot-name value) in overrides do
          (setf (slot-value copy slot-name) value))
    copy))

(defun piece-down (piece)
  (adjusted-piece piece `((y ,(1+ (piece-y piece))))))

(defun piece-left (piece)
  (adjusted-piece piece `((x ,(1- (piece-x piece))))))

(defun piece-right (piece)
  (adjusted-piece piece `((x ,(1+ (piece-x piece))))))

(defun piece-rotate (piece)
  (multiple-value-bind (new-coords new-orientation)
      (next-orientation (piece-id piece) (piece-orientation piece))
    (adjusted-piece piece `((coords ,new-coords)
                        (orientation ,new-orientation)))))

(defparameter piece-moves
  (list #'piece-left
        #'piece-right
        #'piece-down
        #'piece-rotate))

(defconstant +piece-bbox-size+ 4)
(defconstant +n-orientations+ 4)

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

(defun generate-piece (template)
  ;; Generate all of the piece's possible orientations, add
  ;; them to the map of piece -> orientations.
  ;; Then return the piece in its first orientation and starting
  ;; position (bounding box in top left corner).
  (when (not (= +piece-bbox-size+ (length template)))
    (error "Piece is the wrong size."))
  (defun rotate-coords (n x y)
    (if (= n 0)
        (list x y)
        (rotate-coords (1- n) (- +piece-bbox-size+ y 1) x)))
  (let ((coords (parse-piece-template template))
        (piece-id (hash-table-count piece-orientations)))
    (setf (gethash piece-id piece-orientations)
          (loop for num-rotations from 0 below +n-orientations+ collect
                (loop for (x y) in coords collect
                      (rotate-coords num-rotations x y))))
    (make-piece
     :x 0
     :y 0
     :id piece-id
     :coords coords
     :orientation 0)))

(defun parse-piece-template (template)
  (apply #'append
         (loop for y from 0 below +piece-bbox-size+ collect
               (let ((row (nth y template)))
                 (when (not (= +piece-bbox-size+ (length row)))
                   (error "Piece row is wrong size."))
                 (loop for x from 0 below +piece-bbox-size+
                       when (equal #\# (char row x))
                       collect (list x y))))))

;; Order is important, as described in
;; the original HATETRIS code.
(defparameter +pieces+
  (mapcar
   #'generate-piece
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
      "...."))))
;; things to do:
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
