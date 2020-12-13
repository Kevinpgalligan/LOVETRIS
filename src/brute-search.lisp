(in-package lovetris)

(defclass brute-searcher ()
  ((search-depth
    :initarg :search-depth
    :initform 4
    :reader search-depth
    :documentation "How deep to search in the tree before picking a move.")
   (heuristic-eval
    :initarg :heuristic-eval
    :reader heuristic-eval
    :initform (error "Must supply heuristic evaluator.")
    :documentation "A function that estimates the 'goodness' of a state, should return a real number. Higher is better.")
   (root-node
    :accessor root-node)
   (cache
    :accessor cache)))

(defmethod initialize-instance :after ((searcher brute-searcher)
                                       &key start-state)
  (when (not start-state)
    (error "No state to start from."))
  (let ((root (make-instance 'node :state start-state)))
    (setf (root-node searcher) root)
    (setf (cache searcher) (make-node-cache root))))

(defmethod advance ((searcher brute-searcher))
  (expand-tree! searcher)
  (propagate-heuristics! searcher)
  (let* ((best-edge
           (alexandria:extremum (edges (root-node searcher))
                                #'>
                                :key (lambda (edge)
                                       (heuristic-value (edge-child edge)))))
         (new-root (edge-child best-edge)))
    (setf (cache searcher) (make-node-cache new-root))
    (loop for node in (children (root-node searcher)) do
          ;; This should help SBCL to perform garbage collection
          ;; properly. In large data structures, such as trees, SBCL's
          ;; garbage collector can leave entire branches of the tree
          ;; uncollected after we've discarded them, due to its conservatism.
          ;; Eventually, this will cause heap exhaustion. A rather nasty
          ;; trait of the implementation.
          (when (not (eq node new-root))
            (destroy-tree node (cache searcher))))
    (setf (root-node searcher) new-root)
    (values (state new-root) (edge-move-sequence best-edge))))

(defun expand-tree! (searcher)
  (with-accessors ((search-depth search-depth)
                   (root root-node))
      searcher
    (traverse-dag root
                  (lambda (node)
                    (when (not (expanded node))
                      (expand-node! searcher node)))
                  :order :pre
                  ;; Reduce by 1, we don't want to expand
                  ;; the leaves.
                  :max-depth (1- search-depth))))

(defun expand-node! (searcher node)
  (with-accessors ((cache cache))
      searcher
    (setf (edges node)
          (loop for placement in (possible-placements (state node))
                for child = (make-instance 'node
                                           :state (state placement)
                                           :parents (list node))
                for existing = (get-node cache child)
                do (if existing
                       (setf (parents existing) (cons node (parents existing)))
                       (add cache child))
                collect (make-edge (move-sequence placement) (or existing child))))))

(defun propagate-heuristics! (searcher)
  (with-accessors ((root root-node)
                   (heuristic-eval heuristic-eval))
      searcher
    (traverse-dag root
                  (lambda (node)
                    (let ((children (children node)))
                      (setf (heuristic-value node)
                            (if children
                                (apply #'max (mapcar #'heuristic-value children))
                                (funcall heuristic-eval (state node))))))
                  :order :post)))
