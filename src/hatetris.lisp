;;;; The base game.
;;;; Game state, pieces, piece moves, generating valid
;;;; next states given a piece, and so on.

(in-package lovetris)

(defconstant +empty+ 0)
(defconstant +full+ 1)

(defconstant +bbox-size+ 4)
(defconstant +n-orientations+ 4)

;; Should really make this a class and
;; hide the internals. Oh well.
(defstruct state well score bar game-over)

(defun state-hash (state)
  (+ (* 2 (well-hash state))
     (* 3 (state-score state))
     (* 5 (state-bar state))
     (* 7 (if (state-game-over state) 1 0))))

(defparameter *primes*
  (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43
        47 53 59 61 67 71 73 79 83 89 97 101 103
        107 109 113 127 131 137 139 149 151 157
        163 167 173 179 181 191 193 197 199))

(defun well-hash (state)
  ;; Treat rows as binary numbers. Multiply
  ;; them by primes. And sum them.
  ;; THIS WON'T BE ABLE TO PROCESS ALL OF THE ROWS IF
  ;; THERE ARE MORE ROWS THAN THE NUMBER OF PRIMES
  ;; IN *primes*. I've tried to add enough primes for
  ;; any occasion, but you never know.
  (mod (loop for y from (state-bar state) below (well-height state)
             for prime in *primes*
             sum (* prime
                    (loop for x from 0 below (well-width state)
                          sum (if (equalp +empty+ (get-square state x y))
                                  0
                                  (expt 2 x)))))
       (expt 10 20)))

(defun deep-copy-state (state)
  (make-state :well (alexandria:copy-array (state-well state))
              :score (state-score state)
              :bar (state-bar state)
              :game-over (state-game-over state)))

;; A piece has a 4x4 bounding box.
;; We track the x,y coordinates of the upper
;; left corner of the bounding box and the orientation
;; of the piece within that bounding box, which basically
;; determines which squares within the bounding box are
;; non-empty.
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
  (multiple-value-bind (new-orientation new-coords)
      (next-orientation (piece-id piece) (piece-orientation piece))
    (adjusted-piece piece `((coords ,new-coords)
                            (orientation ,new-orientation)))))

(defparameter *piece-moves*
  (list #'piece-left
        #'piece-right
        #'piece-down
        #'piece-rotate))

(defun new-state (&key (width 10) (height 20) (bar 4))
  (make-state :well (make-array (list height width) :initial-element +empty+)
              :score 0
              :bar bar
              :game-over nil))

(defun get-square (state x y)
  (aref (state-well state) y x))

(defun set-square! (state x y new-square)
  (setf (aref (state-well state) y x) new-square))

(defun well-width (state)
  (array-dimension (state-well state) 1))

(defun well-height (state)
  (array-dimension (state-well state) 0))

(defun tower-height (state)
  (loop for y from (1- (well-height state)) downto 0
        while (not (row-empty-p state y))
        finally (return 
                  (- (well-height state)
                     y
                     1))))

(defun merge-piece (state piece)
  ;; Override the name of 'state' to avoid confusion, don't
  ;; want the copy & the original in the same namespace.
  (let ((state (deep-copy-state state)))
    (loop for (x y) in (piece-absolute-coords piece) do
          (set-square! state x y +full+))
    ;; Don't clear filled rows if the tower is above
    ;; the bar, as per the original HATETRIS.
    (if (tower-above-bar state)
        (setf (slot-value state 'game-over) t)
        (clear-filled-rows! state))
    state))

(defun tower-above-bar (state)
  (not (row-empty-p state (1- (state-bar state)))))

(defun clear-filled-rows! (state)
  (let* ((filled-rows
           (loop for y from 0 below (well-height state)
                 when (row-full-p state y)
                 sum 1))
         (new-score (+ filled-rows (state-score state))))
    (setf (slot-value state 'score) new-score)
    ;; Clear full rows and move down the tower.
    (loop for y from 0 below (well-height state) do
          (when (row-full-p state y)
            (loop for x from 0 below (well-width state) do
                  (set-square! state x y +empty+))
            (loop for y-above from (1- y) downto 0 do
                  (loop for x from 0 below (well-width state) do
                        (when (equalp +full+ (get-square state x y-above))
                          ;; Move the square down.
                          (set-square! state x y-above +empty+)
                          (set-square! state x (1+ y-above) +full+))))))))

(defun row-full-p (state y)
  (row-does-not-contain +empty+ state y))

(defun row-empty-p (state y)
  (row-does-not-contain +full+ state y))

(defun row-does-not-contain (square-type state y)
  (not (loop for x from 0 below (well-width state)
             thereis (equalp square-type (get-square state x y)))))

(defun get-placements (state piece)
  ;; Move piece down to the area of interest so that we
  ;; don't waste time.
  (loop while (bbox-above-tower state piece) do
        (setf piece (piece-down piece)))
  ;; Keep track of piece positions / orientations we've
  ;; seen before.
  (let ((seen (make-hash-table :test #'equal)))
    ;; Now do a depth first search to find valid piece positions
    ;; using the left, right, down & rotate movements. Have to keep
    ;; track of positions we've seen before, otherwise we'd end up
    ;; in an infinite loop.
    (let ((locked-positions (list))
          (positions (list piece)))
      (loop while positions do
            (let ((piece (pop positions)))
              (when (lockable-p state piece)
                (push piece locked-positions))
              (loop for move in *piece-moves* do
                    (let ((next-position (funcall move piece)))
                      (when (and (not (seen-p seen next-position))
                                 (valid-position-p state next-position))
                        (add-to-seen seen next-position)
                        (push next-position positions))))))
      (mapcar (lambda (piece) (merge-piece state piece))
              ;; Some of the positions are duplicates, it's just
              ;; that the bounding box is in a different position
              ;; and the piece itself is in an equivalent but
              ;; translated orientation.
              (remove-duplicate-positions locked-positions)))))

(defun seen-p (seen piece)
  (gethash (piece-key piece) seen))

(defun add-to-seen (seen piece)
  (setf (gethash (piece-key piece) seen) t))

(defun remove-duplicate-positions (positions)
  ;; May be inefficient to use a list here, it can be
  ;; optimised if necessary. There shouldn't be more than 10s
  ;; of positions, anyway.
  (let ((uniques (list)))
    (loop for position in positions do
          (setf uniques
                (adjoin position uniques :test #'are-duplicates)))
    uniques))

(defun are-duplicates (p1 p2)
  ;; Consider the positions to be equal
  ;; if they have all the same absolute
  ;; coordinates (not necessarily in the
  ;; same order).
  (let ((p2-absolute-coords (piece-absolute-coords p2)))
    (every (lambda (x-y-pair)
             (find x-y-pair p2-absolute-coords :test #'equalp))
           (piece-absolute-coords p1))))

(defun valid-position-p (state piece)
  (not (loop for (x y) in (piece-absolute-coords piece)
             thereis (or (outside-well-p state x y)
                         (overlaps-p state x y)))))

(defun outside-well-p (state x y)
  (or (< x 0)
      (< y 0)
      (>= x (well-width state))
      (>= y (well-height state))))

(defun overlaps-p (state x y)
  (equalp +full+
          (get-square state x y)))

;; A piece can be locked in place if it's resting on
;; something else, e.g. the bottom of the well or a
;; filled-in square in the well.
(defun lockable-p (state piece)
  (loop for (x y) in (piece-absolute-coords piece)
        thereis (let ((below-y (1+ y)))
                  (or (>= below-y (well-height state))
                      (overlaps-p state x below-y)))))

(defun piece-key (piece)
  (list (piece-x piece)
        (piece-y piece)
        (piece-orientation piece)))

(defun bbox-above-tower (state piece)
  (let ((y-below-bbox (+ (piece-y piece) +bbox-size+)))
    (and (< y-below-bbox (well-height state))
         (row-empty-p state y-below-bbox))))

;; Normally, coordinates are relative to the position
;; of the bounding box. This returns the absolute version.
(defun piece-absolute-coords (piece)
  (loop for (x y) in (piece-coords piece) collect
        (list (+ x (piece-x piece))
              (+ y (piece-y piece)))))

(defparameter *piece-orientations*
  (make-hash-table :test #'equal))

(defun get-orientations (piece-id)
  (gethash piece-id *piece-orientations*))

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
  (when (not (= +bbox-size+ (length template)))
    (error "Piece is the wrong size."))
  (defun rotate-coords (n x y)
    (if (= n 0)
        (list x y)
        (rotate-coords (1- n) (- +bbox-size+ y 1) x)))
  (let ((coords (parse-piece-template template))
        (piece-id (hash-table-count *piece-orientations*)))
    (setf (gethash piece-id *piece-orientations*)
          ;; I considered removing translations here, but it's
          ;; possible that the only way to manoeuvre into a
          ;; position is by going through one of the redundant
          ;; orientations. Also, it's best to keep the code as
          ;; close to the original HATETRIS as possible.
          ;; Translations can be removed later, as an optimisation,
          ;; if I can convince myself that all of the same placements
          ;; will be possible.
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
         (loop for y from 0 below +bbox-size+ collect
               (let ((row (nth y template)))
                 (when (not (= +bbox-size+ (length row)))
                   (error "Piece row is wrong size."))
                 (loop for x from 0 below +bbox-size+
                       when (equal #\# (char row x))
                       collect (list x y))))))

(defun get-pieces ()
  (mapcar #'copy-structure
          *pieces*))

;; Order is important, as described in
;; the original HATETRIS code.
(defparameter *pieces*
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
