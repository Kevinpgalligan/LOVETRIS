;;;; Optimisation by genetic algorithm.
;;;;
;;;; What must be defined to use it:
;;;;  - a genotype class, implementing the methods below.
;;;;  - a no-args function that returns a random genotype.
;;;;
;;;; By genotype, I mean the properties of a solution.
;;;; For example, in a solution to maximize the fit of a
;;;; quadratic equation to some datapoints, the genotype would
;;;; be the coefficients of the equation.

(defpackage :genetic
  (:use :cl)
  (:export
   #:random-pop
   #:evolve
   
   ;; Used with population.
   #:best-solution
   #:fitness-history
   #:solutions
   #:size

   ;; Attributes of solutions.
   #:genotype
   #:fitness

   #:crossover
   #:mutate))

(in-package genetic)

;;; Genotype class must be implemented with definitions
;;; of these methods.
(defgeneric crossover (genotype1 genotype2)
  (:documentation "Combines 2 genotypes to make a new one."))
(defgeneric mutate (genotype)
  (:documentation "Returns a randomly modified version of the given genotype."))

(defclass solution ()
  ((genotype
    :initarg :genotype
    :initform (error "Must provide genotype.")
    :reader genotype)
   (fitness
    :initarg :fitness
    :initform (error "Must provide solution's fitness score.")
    :reader fitness)))

(defclass population ()
  ((solutions
    :initarg :solutions
    :initform (error "Must provide initial solutions.")
    :accessor solutions
    :documentation "The current solutions in the population.")
   (size
    :reader size)
   (fitness-history
    :initform nil
    :accessor fitness-history)))

#|
(defun serialize-population (pop filename)
  "If you wanna call this function, you have to implement
the SERIALIZE-GENOTYPE method."
  (let ((history-size (length (fitness-history pop))))
    (with-open-file (f filename
                       :direction :output
                       :if-exists :supersede)
      (format f "~a ~a~%"))))
|#

(defmethod initialize-instance :after ((population population) &key)
  (add-to-fitness-history population (solutions population))
  (setf (slot-value population 'size) (length (solutions population))))

(defun add-to-fitness-history (population solutions)
  (setf (fitness-history population)
        (append (fitness-history population)
                (list (mapcar #'fitness solutions)))))

(defun best-solution (population)
  (alexandria:extremum (solutions population) #'> :key #'fitness))

(defun update-solutions! (population new-solutions)
  (setf (solutions population) new-solutions)
  (add-to-fitness-history population new-solutions))

(defun random-pop (random-genotype pop-size eval-fitness)
  (make-instance 'population
                 :solutions (mapcar
                             (lambda (genotype)
                               (make-instance 'solution
                                              :genotype genotype
                                              :fitness (funcall eval-fitness genotype)))
                             (loop repeat pop-size
                                   collect (funcall random-genotype)))))

(defun evolve (population
               eval-fitness
               &key
                 (rounds 5)
                 (operations '((:crossover 70)
                               (:mutation 20)
                               (:elitism 10)))
                 (log t))
  (assert eval-fitness)
  (dotimes (i rounds population)
    (when log
      (format t "Round #~a, best solution fitness ~a~%" i (fitness (best-solution population))))
    (evolve-solutions! population eval-fitness operations)))

(defun evolve-solutions! (population eval-fitness operations)
  "Creates new solutions based on genetic operations and overwrites the old ones
in POPULATION."
  (let ((n (size population))
        (total-prop (loop for (op proportion) in operations
                          sum proportion)))
    (update-solutions!
     population
     (apply #'append
            (loop for (op proportion) in operations
                  for i from (1- (length operations)) downto 0
                  collect (let ((m (if (= i 0)
                                       ;; This prevents the population size being
                                       ;; off by 1.
                                       n
                                       (floor (* n (/ proportion total-prop))))))
                            (decf n m)
                            (create-solutions op m population eval-fitness)))))))

(defun create-solutions (op m population eval-fitness)
  "Create M new solutions based on POPULATION using the operation OP."
  (cond
    ((eq op :mutation)
     (loop repeat m collect
           (let ((genotype (mutate (genotype (select-solution population)))))
             (make-instance 'solution
                            :genotype genotype
                            :fitness (funcall eval-fitness genotype)))))
    ((eq op :crossover)
     (loop repeat m collect
           (let ((genotype
                   (crossover (genotype (select-solution population))
                              (genotype (select-solution population)))))
             (make-instance 'solution
                            :genotype genotype
                            :fitness (funcall eval-fitness genotype)))))
    ((eq op :elitism)
     (subseq (sort (solutions population)
                   #'>
                   :key #'fitness)
             0
             m))
    (t (error "Unknown genetic operator ~S" op))))

(defun select-solution (population)
  "Tournament selection."
  (let ((s1 (random-solution population))
        (s2 (random-solution population)))
    (if (> (fitness s1) (fitness s2))
        s1
        s2)))

(defun random-solution (population)
  (nth (random (size population))
       (solutions population)))
