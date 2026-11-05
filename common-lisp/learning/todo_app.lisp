(defparameter *db* (make-array 0 :adjustable t :fill-pointer 0))

(defstruct entry done description)

(defun add-entry (e)
    (vector-push-extend e *db*))

(defun mark-done (idx)
    (when (< idx (length *db*))
        (let ((el (aref *db* idx)))
             (setf (entry-done el) t)
             (setf (aref *db* idx) el))))

(defun print-db ()
    (loop for i from 0 below (length *db*)
        do (format t "~a~a~%" i (aref *db* i))))
