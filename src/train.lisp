(in-package lovetris)

(defconstant +max-states+ 100)
(defconstant +mutate-max+ 0.5)

(defclass search-genotype ()
  ((heuristic-params
    :initarg :heuristic-params
    :reader heuristic-params)))

(defmethod print-object ((search-genotype search-genotype) stream)
  (prin1 (heuristic-params search-genotype) stream))

(defun parse-search-genotype (params)
  (make-instance 'search-genotype
                 :heuristic-params params))

(defmethod genetic:crossover ((geno1 search-genotype)
                              (geno2 search-genotype))
  (make-instance 'search-genotype
                 :heuristic-params (normalise
                                    (loop for x in (heuristic-params geno1)
                                          for y in (heuristic-params geno2)
                                          collect (/ (+ x y) 2)))))

(defmethod genetic:mutate ((geno search-genotype))
  (make-instance 'search-genotype
                 :heuristic-params (normalise
                                    (loop for x in (heuristic-params geno)
                                          collect (+ x
                                                     ;; Difference can be positive or
                                                     ;; negative.
                                                     (- (random (* 2 +mutate-max+))
                                                        +mutate-max+))))))

(defun normalise (params)
  "Move params onto unit circle."
  (let ((dist (sqrt
               (apply #'+
                      (mapcar (lambda (x)
                                (* x x))
                              params)))))
    (loop for x in params collect
          (/ x dist))))

(defun make-searcher-eval (&key (search-depth 4) (num-threads 4) (max-states +max-states+))
  (lambda (genotype)
    (run-searcher
     (lambda (state)
       (make-instance 'brute-searcher
                      :search-depth search-depth
                      :heuristic-eval (apply #'get-heuristic-eval
                                             (heuristic-params genotype))
                      :start-state state
                      :num-threads num-threads))
     :max-states max-states)))

(defun random-searcher-pop (pop-size eval-fn)
  (genetic:random-pop
   (lambda ()
     (make-instance 'search-genotype
                    :heuristic-params (normalise
                                       (list (- (random 1.0))
                                             (random 1.0)
                                             (- (random 1.0))
                                             (- (random 1.0))))))
   pop-size
   eval-fn))
