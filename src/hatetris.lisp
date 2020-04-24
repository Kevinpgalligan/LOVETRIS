;;;; The base game.
;;;; Game state, pieces, piece moves, generating valid
;;;; next states given a piece, and so on.

(in-package lovetris)

(defconstant +bbox-size+ 4)
(defconstant +n-orientations+ 4)

(defconstant +well-width+ 10)
(defconstant +well-height+ 20)
(defconstant +bar+ 4)

(defclass state ()
  ((well
    :initarg :well
    :initform (error "Must define well.")
    :reader well)
   (score
    :initarg :score
    :accessor score)
   (bar
    :initarg :bar
    :reader bar)
   (game-over
    :initarg :game-over
    :accessor game-over)
   (last-move-sequence
    :initarg :last-move-sequence
    :initform nil
    :accessor last-move-sequence)))

(defun make-state ()
  (make-instance 'state
                 :well (make-array +well-height+
                                   :element-type `(unsigned-byte ,+well-width+)
                                   :initial-element 0)
                 :score 0
                 :bar +bar+
                 :game-over nil
                 :last-move-sequence nil))

(defun full-square-p (state x y)
  (< 0 (logand (ash 1 x) (aref (well state) y))))

(defun empty-square-p (state x y)
  (not (full-square-p state x y)))

(defun fill-square! (state x y)
  (setf (aref (well state) y)
        (logior (aref (well state) y)
                (ash 1 x))))

(defun clear-square! (state x y)
  (setf (aref (well state) y)
        (logand (aref (well state) y)
                (lognot (ash 1 x)))))

(defun clear-row! (state y)
  (setf (aref (well state) y) 0))

(defun shift-row-down! (state y n)
  "Shift a row down by N."
  (setf (aref (well state) (+ y n))
        (aref (well state) y))
  (clear-row! state y))

(defun row-full-p (state y)
  ;; Some bitwise trickery.
  ;; Shift operation on 1 gives something
  ;; like 100000, decrementing gives 11111.
  (= (1- (ash 1 (well-width state)))
     (aref (well state) y)))

(defun row-empty-p (state y)
  (= 0 (aref (well state) y)))

(defun well-width (state)
  ;; I was trying to make the implementation flexible
  ;; enough to support any well size, but now I've given
  ;; up on that.
  (declare (ignore state))
  +well-width+)

(defun well-height (state)
  (array-total-size (well state)))

(defun tower-height (state)
  (loop for y from (1- (well-height state)) downto 0
        while (not (row-empty-p state y))
        finally (return
                  (- (well-height state)
                     y
                     1))))

(defun state-hash (state)
  (+ (* 2 (well-hash state))
     (* 3 (score state))
     (* 5 (bar state))
     (* 7 (if (game-over state) 1 0))))

(defparameter *primes*
  (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43
        47 53 59 61 67 71 73 79 83 89 97 101 103
        107 109 113 127 131 137 139 149 151 157
        163 167 173 179 181 191 193 197 199))

(defun well-hash (state)
  ;; Multiply rows by primes and sum them.
  ;; THIS WON'T BE ABLE TO PROCESS ALL OF THE ROWS IF
  ;; THERE ARE MORE ROWS THAN THE NUMBER OF PRIMES
  ;; IN *primes*. I've tried to add enough primes for
  ;; any occasion, but you never know.
  (mod (loop for row across (well state)
             for prime in *primes*
             sum (* prime row))
       (expt 10 20)))

(defun deep-copy-state (state)
  (make-instance 'state
                 :well (alexandria:copy-array (well state))
                 :score (score state)
                 :bar (bar state)
                 :game-over (game-over state)))

(defun tower-above-bar (state)
  (not (row-empty-p state (1- (bar state)))))

(defun clear-full-rows! (state)
  "Updates well & score of state."
  (let ((full-rows 0))
    (loop for y from (1- (well-height state)) downto 0 do
          (cond
            ((row-full-p state y)
             (progn
               (clear-row! state y)
               (incf full-rows)))
            ((row-empty-p state y)
             ;; We've passed the top of the tower, no more
             ;; rows to clear or shift down.
             (return))
            ((> full-rows 0)
             (shift-row-down! state y full-rows))))
    (incf (score state)
          full-rows)))

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
  (list
   (list #'piece-down "D")
   (list #'piece-left "L")
   (list #'piece-right "R")
   (list #'piece-rotate "U")))

(defun merge-piece (state piece &optional move-sequence)
  ;; Override the name of 'state' to avoid confusion, don't
  ;; want the copy & the original in the same namespace.
  (let ((state (deep-copy-state state)))
    (loop for (x y) in (piece-absolute-coords piece) do
          (fill-square! state x y))
    ;; Don't clear full rows if the tower is above
    ;; the bar, as per the original HATETRIS.
    (if (tower-above-bar state)
        (setf (slot-value state 'game-over) t)
        (clear-full-rows! state))
    (setf (slot-value state 'last-move-sequence) move-sequence)
    state))

(defun possible-next-states (state)
  (if (game-over state)
      (list)
      (multiple-value-bind (piece placements)
          (get-worst-piece state)
        (declare (ignore piece))
        placements)))

(defun get-placements (state piece)
  ;; Move piece down to the area of interest so that we
  ;; don't waste time.
  (let ((initial-move-sequence (list)))
    (loop while (bbox-above-tower state piece) do
          (setf piece (piece-down piece))
          (push "D" initial-move-sequence))
    ;; Keep track of piece positions / orientations we've
    ;; seen before.
    (let ((seen (make-hash-table :test #'equal)))
      ;; Now do a breadth first search to find valid piece positions
      ;; using the left, right, down & rotate movements. Have to keep
      ;; track of positions we've seen before, otherwise we'd end up
      ;; in an infinite loop. Breadth first search is preferred over
      ;; depth first search because it gives the shortest move sequence
      ;; to reach each position.
      (let ((locked-positions (list))
            (positions (queue:make-queue
                        :initial-contents (list (list piece initial-move-sequence)))))
        (loop while (not (queue:queue-empty-p positions)) do
              (let* ((position (queue:queue-pop positions))
                     (piece (car position))
                     (move-sequence (cadr position)))
                (when (lockable-p state piece)
                  ;; One extra move is required to lock it in place.
                  (push (list piece (cons "D" move-sequence))
                        locked-positions))
                (loop for (move move-id) in *piece-moves* do
                      (let ((next-piece (funcall move piece)))
                        (when (and (not (seen-p seen next-piece))
                                   (valid-position-p state next-piece))
                          (add-to-seen seen next-piece)
                          (queue:queue-push
                           (list next-piece (cons move-id move-sequence))
                           positions))))))
        ;; I HATE the code in this function, need to refactor it.
        ;; If it helps to clarify for my future self, what I have referred
        ;; to as a "position" here is a list containing a piece and the move
        ;; sequence that was necessary to get it there.
        (mapcar (lambda (position)
                  (apply (lambda (piece move-sequence)
                           (merge-piece state piece move-sequence))
                         position))
                ;; Some of the positions are duplicates, it's just
                ;; that the bounding box is in a different position
                ;; and the piece itself is in an equivalent but
                ;; translated orientation.
                (remove-duplicate-positions locked-positions))))))

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
          (setf uniques (add-if-unique position uniques)))
    uniques))

(defun add-if-unique (position uniques)
  ;; Using custom code here so that a position can be
  ;; swapped for an equivalent one when the equivalent
  ;; one has a shorter move sequence. Makes the replay
  ;; look nicer.
  (if (not uniques)
      (list position)
      (let ((next (car uniques)))
        (cond
          ((not (equivalent-pieces (car next) (car position)))
           (cons next
                 (add-if-unique position (cdr uniques))))
          ((>= (length (cadr position))
               (length (cadr next)))
           uniques)
          (t
           (cons position (cdr uniques)))))))

(defun equivalent-pieces (p1 p2)
  ;; Consider the pieces to be equal
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
                         (full-square-p state x y)))))

(defun outside-well-p (state x y)
  (or (< x 0)
      (< y 0)
      (>= x (well-width state))
      (>= y (well-height state))))

;; A piece can be locked in place if it's resting on
;; something else, e.g. the bottom of the well or a
;; filled-in square in the well.
(defun lockable-p (state piece)
  (loop for (x y) in (piece-absolute-coords piece)
        thereis (let ((below-y (1+ y)))
                  (or (>= below-y (well-height state))
                      (full-square-p state x below-y)))))

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

(defun get-worst-piece (state)
  ;; Important: if 2 pieces have the same score, this will
  ;; return the one that appears first. Necessary to mimic
  ;; the original HATETRIS.
  (let ((best-piece-placements-pair
          (first-max (mapcar (lambda (piece)
                               (list piece (get-placements state piece)))
                             (get-pieces))
                     ;; Return the piece with the max minimum height.
                     (lambda (piece-placements-pair)
                       (min-height (second piece-placements-pair))))))
    (values (first best-piece-placements-pair)
            (second best-piece-placements-pair))))

(defun first-max (xs key)
  ;; Have to reverse the list first, otherwise it'll return the
  ;; last x to have the max value. It's annoying that this takes
  ;; so much effort.
  (alexandria:extremum (reverse xs) #'> :key key))

(defun min-height (placements)
  (apply #'min (mapcar #'tower-height placements)))

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
     :x (/ (- +well-width+ +bbox-size+) 2)
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

(defun get-pieces ()
  (mapcar #'copy-structure
          *pieces*))
