(defpackage :queue
  (:use :cl)
  (:export
   #:make-queue

   #:qpush
   #:qpop
   #:empty-p))

(in-package :queue)

(defclass queue ()
  ((head
    :initarg :head
    :accessor head)
   (tail
    :initarg :tail
    :accessor tail)))

(defun make-queue (&key initial-contents)
  (make-instance 'queue
                 :head initial-contents
                 :tail (last initial-contents)))

(defgeneric qpush (queue item))
(defgeneric qpop (queue))
(defgeneric empty-p (queue))

(defmethod qpush ((queue queue) item)
  (let ((new-cell (cons item nil)))
    (set (tail queue) new-cell)
    (if (empty-p queue)
        (setf (head queue) new-cell)
        (rplacd (tail queue) new-cell))))

(defmethod qpop ((queue queue))
  (let ((current-head (head queue)))
    (setf (head queue) (cdr current-head))
    (car current-head)))

(defmethod empty-p ((queue queue))
  (or (not (head queue))
      (not (tail queue))))
