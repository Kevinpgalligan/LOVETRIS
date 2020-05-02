(in-package lovetris)

(defconstant +max-states+ 250)
(defconstant +mutate-max+ 0.5)

(defclass search-genotype ()
  ((heuristic-params
    :initarg :heuristic-params
    :reader heuristic-params)
   (disable-beam
    :initarg :disable-beam
    :reader disable-beam)
   (beam-width
    :initarg :beam-width
    :reader beam-width)
   (search-depth
    :initarg :search-depth
    :reader search-depth)
   (num-threads
    :initarg :num-threads
    :reader num-threads)))

(defmethod genetic:eval-fitness ((geno search-genotype))
  (run-searcher
   (lambda (state)
     (make-instance 'beam-searcher
                    :disable-beam (disable-beam geno)
                    :beam-width (beam-width geno)
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
                 :disable-beam (disable-beam geno1)
                 :beam-width (beam-width geno1)
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
                 :disable-beam (disable-beam geno)
                 :beam-width (beam-width geno)
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
                                    (disable-beam nil)
                                    (beam-width 2)
                                    (search-depth 4)
                                    (num-threads 1))
  (genetic:evolve-random
   (lambda ()
     (make-instance 'search-genotype
                    :heuristic-params (normalise
                                       (list (- (random 1.0))
                                             (random 1.0)
                                             (- (random 1.0))
                                             (- (random 1.0))))
                    :disable-beam disable-beam
                    :beam-width beam-width
                    :search-depth search-depth
                    :num-threads num-threads))
   :pop-size pop-size
   :rounds rounds))

(defun plot-fitness (population)
  (vgplot:plot (apply #'append
                      (loop for fs in (genetic:fitness-history population)
                            for i = 0 then (1+ i)
                            collect (loop repeat (length fs)
                                          collect i)))
               (apply #'append
                      (genetic:fitness-history population))
               "+;"))
