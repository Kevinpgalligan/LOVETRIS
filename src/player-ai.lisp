;;;; AI to place the pieces.

(in-package lovetris)

(defun place-randomly (state piece)
  (let ((options (get-placements state piece)))
    (nth (random (length options))
         options)))
