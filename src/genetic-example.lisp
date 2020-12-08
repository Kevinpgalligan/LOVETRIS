(defpackage :genetic-example
  (:use :cl :genetic)
  (:export
   #:evolve-eqn))

(in-package genetic-example)

(defclass eqn ()
  ((coeffs
    :initarg :coeffs
    :reader coeffs)))

(defun eval-eqn-fitness (eqn)
  (destructuring-bind (a b c)
      (coeffs eqn)
    (let ((diff (loop for (x y) in '((1 2) (2 3) (3 2))
                      sum (abs (- y (+ (* a (* x x))
                                       (* b x)
                                       c))))))
      (if (zerop diff)
          10000000 ; hack
          (/ 1 diff)))))

(defmethod mutate ((eqn eqn))
  (make-instance 'eqn
                 :coeffs (loop for a in (coeffs eqn)
                               collect (+ a (- (random 5.0) 2.5)))))

(defun evolve-eqn ()
  (evolve (random-pop
           (lambda ()
             (make-instance 'eqn
                            :coeffs (loop repeat 3 collect
                                          (- 10 (random 20.0)))))
           100
           #'eval-eqn-fitness)
          #'eval-eqn-fitness
          :rounds 250
          :operations '((:mutation 90)
                        (:elitism 10))))
