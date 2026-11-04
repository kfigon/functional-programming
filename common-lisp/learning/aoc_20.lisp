(defun d1p1 (data) 
    (let ((set (make-hash-table :test #'equal)))
        (dolist (x data) (setf (gethash x set) t)) 
        (dolist (x data)
            (let ((to-find (- 2020 x)))
                (when (gethash to-find set)
                    (return-from d1p1 (* to-find x)))))))

(defun read-file (path)
    (with-open-file (in path)
        (loop for line = (read-line in nil)
            while line
            collect line)))
(defun parse (data)
    (mapcar #'parse-integer data))

(defparameter *parsed-data* (parse (read-file "resources/aoc_d1_20.txt")))
(print (d1p1 *parsed-data*))
