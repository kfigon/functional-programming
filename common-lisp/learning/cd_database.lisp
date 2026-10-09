(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

; convention for global variables
(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
    (dolist (cd *db*)
        (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
    (format *QUERY-IO* "~a: " prompt)
    (force-output *QUERY-IO*)
    (read-line *QUERY-IO*))

(defun prompt-for-cd ()
    (make-cd 
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
    (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
    (with-open-file (out filename
                     :direction :output
                     :if-exists :supersede)
                     (with-standard-io-syntax (print *db* out))))


(defun load-db (filename)
    (with-open-file (in filename)
        (with-standard-io-syntax (setf *db* (read in)))))

; next Querying the Database

; (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
; (add-record (make-cd "Fly" "Crazy stuff" 1 nil))
; (add-record (make-cd "Low" "The piglets" 3 nil))
