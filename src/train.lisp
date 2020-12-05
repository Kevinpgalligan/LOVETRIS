(in-package lovetris)

(defconstant +max-states+ 250)
(defconstant +mutate-max+ 0.5)

(defclass search-genotype ()
  ((heuristic-params
    :initarg :heuristic-params
    :reader heuristic-params)
   (search-depth
    :initarg :search-depth
    :reader search-depth)
   (num-threads
    :initarg :num-threads
    :reader num-threads)))

(defmethod genetic:eval-fitness ((geno search-genotype))
  (run-searcher
   (lambda (state)
     (make-instance 'brute-searcher
                    :search-depth (search-depth geno)
                    :heuristic-eval (apply #'get-heuristic-eval
                                           (heuristic-params geno))
                    :start-state state
                    :num-threads (num-threads geno)))
   :max-states +max-states+))

(defmethod genetic:crossover ((geno1 search-genotype)
                              (geno2 search-genotype))
  (make-instance 'search-genotype
                 :heuristic-params (normalise
                                    (loop for x in (heuristic-params geno1)
                                          for y in (heuristic-params geno2)
                                          collect (/ (+ x y) 2)))
                 :search-depth (search-depth geno1)
                 :num-threads (num-threads geno1)))

(defmethod genetic:mutate ((geno search-genotype))
  (make-instance 'search-genotype
                 :heuristic-params (normalise
                                    (loop for x in (heuristic-params geno)
                                          collect (+ x
                                                     ;; Difference can be positive or
                                                     ;; negative.
                                                     (- (random (* 2 +mutate-max+))
                                                        +mutate-max+))))
                 :search-depth (search-depth geno)
                 :num-threads (num-threads geno)))

(defun normalise (params)
  "Move params onto unit circle."
  (let ((dist (sqrt
               (apply #'+
                      (mapcar (lambda (x)
                                (* x x))
                              params)))))
    (loop for x in params collect
          (/ x dist))))

(defun evolve-searcher (rounds &key (pop-size 100)
                                    (search-depth 4)
                                    (num-threads 1)
                                    log)
  (genetic:evolve-random
   (lambda ()
     (make-instance 'search-genotype
                    :heuristic-params (normalise
                                       (list (- (random 1.0))
                                             (random 1.0)
                                             (- (random 1.0))
                                             (- (random 1.0))))
                    :search-depth search-depth
                    :num-threads num-threads))
   :pop-size pop-size
   :rounds rounds
   :log log))

(defun plot-fitness (population)
  (vgplot:plot (apply #'append
                      (loop for fs in (genetic:fitness-history population)
                            for i = 0 then (1+ i)
                            collect (loop repeat (length fs)
                                          collect i)))
               (apply #'append
                      (genetic:fitness-history population))
               "+;"))
