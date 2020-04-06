;;;; Contains the "enemy AI" that's responsible for providing
;;;; the worst possible piece for the next move.

(in-package lovetris)

(defun get-worst-piece (state)
  ;; Important: if 2 pieces have the same score, this will
  ;; return the one that appears first. Necessary to mimic
  ;; the original HATETRIS.
  (first-max +pieces+
             (lambda (piece)
               (score-piece state piece))))

(defun first-max (xs key)
  ;; Have to reverse the list first, otherwise it'll return the
  ;; last x to have the max value. It's annoying that this takes
  ;; so much effort.
  (alexandria:extremum (reverse xs) #'> :key key))

(defun score-piece (state piece)
  ;; Score based on min tower height achievable. We can then pick the
  ;; worst piece by maximising the minimum tower height.
  ;; Also need a hack... if the game is over, add to the height. That's
  ;; because game over is evaluated before filled rows are removed in the
  ;; original HATETRIS. Not sure if it even makes a difference.
  (apply
   #'min
   (mapcar (lambda (new-state)
             (let ((height (tower-height new-state)))
               (if (state-game-over new-state)
                   (+ 100 height)
                   height)))
           (get-placements state piece))))
