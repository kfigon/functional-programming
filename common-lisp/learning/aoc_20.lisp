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

(defun parse-p1 (data) (mapcar #'parse-integer data))

(defstruct pass-entry
    min max char pass)
; 1-3 a: abcde
; 1-3 b: cdefg
; 2-9 c: ccccccccc
(defun parse-password (line)
    (let* ((dash (position #\- line))
           (space (position #\Space line))
           (colon (position #\: line))
           (min (parse-integer line :end dash))
           (max (parse-integer line :start (+ 1 dash) :end space))
           (character (char line (+ 1 space)))
           (pass (string-trim " " (subseq line (+ 1 colon)))))
        (make-pass-entry :min min :max max :char character :pass pass)))

(defun parse-d2-p1 (data) (mapcar #'parse-password data))

(defun valid-pass-p (pass) 
    (let ((occurence (count (pass-entry-char pass) (pass-entry-pass pass))))
        (and (>= occurence (pass-entry-min pass))
             (<= occurence (pass-entry-max pass)))))

(defun d2p1 (data) (count-if #'valid-pass-p data))


(print (d1p1 (parse-p1 (read-file "resources/aoc_d1_20.txt"))))
(print (d2p1 (parse-d2-p1 (read-file "resources/aoc_d2_20.txt"))))

