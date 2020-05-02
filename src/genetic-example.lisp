(in-package genetic)

(defclass eqn ()
  ((coeffs
    :initarg :coeffs
    :reader coeffs)))

(defmethod eval-fitness ((eqn eqn))
  (let* ((a (car (coeffs eqn)))
         (b (cadr (coeffs eqn)))
         (c (caddr (coeffs eqn)))
         (diff (loop for (x y) in '((1 2) (2 3) (3 2))
                     sum (abs (- y (+ (* a (* x x))
                                      (* b x)
                                      c))))))
    (if (zerop diff)
        10000000 ; hack
        (/ 1 diff))))

(defmethod mutate ((eqn eqn))
  (make-instance 'eqn
                 :coeffs (loop for a in (coeffs eqn)
                               collect (+ a (- (random 5.0) 2.5)))))

(defun evolve-eqn ()
  (evolve-random (lambda ()
                   (make-instance 'eqn
                                  :coeffs (loop repeat 3 collect
                                                (- 10 (random 20.0)))))
                 :pop-size 1000
                 :rounds 100
                 :operations '((mutation 90)
                               (elitism 10))))

(defun plot-fitness (population)
  (vgplot:plot (apply #'append
                      (loop for fs in (fitness-history population)
                            for i = 0 then (1+ i)
                            collect (loop repeat (length fs)
                                          collect i)))
               (apply #'append
                      (fitness-history population))
               "+;"))

