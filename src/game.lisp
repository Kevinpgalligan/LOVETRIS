;;;; An interface for running the game in a loop until completion.

(in-package lovetris)

(defun run-game (&key (pick-piece #'get-worst-piece)
                      (place-piece #'place-randomly))
  (let ((state (new-state)))
    (loop while (not (state-game-over state)) do
          (setf state (funcall place-piece state (funcall pick-piece state)))
          finally (return state))))
