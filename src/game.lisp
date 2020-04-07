;;;; An interface for running the game in a loop until completion.

(in-package lovetris)

(defun do-nothing ())

(defun run-game (&key (pick-piece #'get-worst-piece)
                      (place-piece #'place-randomly)
                      (init-fn #'do-nothing)
                      (cleanup-fn #'do-nothing))
  (funcall init-fn)
  (let ((state (new-state)))
    (unwind-protect
         (loop while (not (state-game-over state)) do
               (setf state (funcall place-piece
                                    state
                                    (funcall pick-piece state)))
               finally (return state))
      (funcall cleanup-fn))))
