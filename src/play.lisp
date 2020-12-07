;;;; Play the game through a GUI.

(in-package lovetris)

(defconstant +border-pixels+ 10)
(defconstant +square-size+ 25)
(defconstant +pixels-for-score+ 50)

(defconstant +window-width+
  (+ (* 2 +border-pixels+)
     (* +well-width+ +square-size+)))
(defconstant +window-height+
  (+ (* 2 +border-pixels+)
     (* +well-height+ +square-size+)
     +pixels-for-score+))

(defparameter *blue* (gamekit:vec4 0 0 1 1))
(defparameter *red* (gamekit:vec4 1 0 0 1))
(defparameter *black* (gamekit:vec4 0 0 0 1))

(defvar *state*)
(defvar *piece*)

(gamekit:defgame hatetris () ()
  (:viewport-title "HATETRIS")
  (:viewport-width +window-width+)
  (:viewport-height +window-height+))

(defmethod gamekit:draw ((app hatetris))
  ;; Draw board.
  (loop for y from 0 below (well-height *state*) do
        (loop for x from 0 below (well-width *state*) do
              (draw-square x
                           y
                           (if (full-square-p *state* x y)
                               *blue*
                               *black*))))
  ;; Draw piece.
  (when *piece*
    (loop for (x y) in (piece-absolute-coords *piece*) do
          (draw-square x y *red*)))
  ;; Draw score.
  (gamekit:draw-text (format nil "Score: ~D" (score *state*))
                     (gamekit:vec2 (* 2 +border-pixels+)
                                   (+ +border-pixels+
                                      (/ +pixels-for-score+ 3))))
  ;; Draw the horizontal bar.
  (let ((bar-y (- +window-height+
                  +border-pixels+
                  (* +bar+ +square-size+)
                  1)))
    (gamekit:draw-rect (gamekit:vec2 +border-pixels+ bar-y)
                       (* +square-size+ +well-width+)
                       1
                       :fill-paint *red*)))

(defmethod gamekit:post-initialize ((app hatetris))
  (loop for (key . move-fn) in `((:up . ,#'piece-rotate)
                                 (:down . ,#'piece-down)
                                 (:left . ,#'piece-left)
                                 (:right . ,#'piece-right))
        do (gamekit:bind-button key :pressed (make-update-fn move-fn))
        do (gamekit:bind-button key :repeating (make-update-fn move-fn))))

(defun make-update-fn (move-fn)
  (lambda ()
    (update-piece move-fn)))

(defun update-piece (piece-move)
  (when *piece*
    (if (and (lockable-p *state* *piece*)
             (eq piece-move #'piece-down))
        (progn
          (setf *state* (merge-piece *state* *piece*))
          (setf *piece*
                (if (game-over *state*)
                    nil
                    (get-worst-piece *state*))))
        (let ((new-piece (funcall piece-move *piece*)))
          (when (valid-position-p *state* new-piece)
            (setf *piece* new-piece))))))

(defun draw-square (x y colour)
  (gamekit:draw-rect (gamekit:vec2 (+ +border-pixels+
                                      (* x +square-size+))
                                   (- +window-height+
                                      1
                                      +border-pixels+
                                      (* (1+ y) +square-size+)))
                     +square-size+
                     +square-size+
                     :fill-paint colour))

(defun play-hatetris ()
  (setf *state* (make-state))
  (setf *piece* (get-worst-piece *state*))
  (gamekit:start 'hatetris))

(defun show-hatetris-replay (encoded-game &key (move-delay-seconds 0.1))
  (play-hatetris)
  (let ((decoded (decode-game encoded-game)))
    (loop for move across decoded
          do (sleep move-delay-seconds)
          do (update-piece (move-to-fn move)))))

(defun move-to-fn (move)
  (cond
    ((char-equal move #\D) #'piece-down)
    ((char-equal move #\L) #'piece-left)
    ((char-equal move #\R) #'piece-right)
    ((char-equal move #\U) #'piece-rotate)
    (t (error (format nil "Invalid move ~C" move)))))

(defun run-hatetris-with-ai (searcher-init &key (move-delay-seconds 0.1))
  (play-hatetris)
  (run-searcher searcher-init
                :initial-state *state*
                :process-fn (lambda (state move-sequence)
                              (declare (ignore state))
                              (loop for move in move-sequence
                                    do (sleep move-delay-seconds)
                                    do (update-piece (move-to-fn move))))))
