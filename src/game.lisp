;;;; Contains code for running the game.

(in-package lovetris)

(defun place-randomly (state piece)
  (let ((options (get-placements state piece)))
    (nth (random (length options))
         options)))

(defun run-game (&key (pick-piece #'get-worst-piece)
                      (place-piece #'place-randomly))
  (let ((state (new-state)))
    (loop while (not (state-game-over state)) do
          (setf state (funcall place-piece
                               state
                               (funcall pick-piece state)))
          finally (return state))))

