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


; #' - lookup for function, not variable. 
; using (remove-if-not evenp list) does not work, but #' works in all cases
(defun select-by-artist (artist)
    (remove-if-not
        #'(lambda (cd) (equal (getf cd :artist) artist))
        *db*))

(defun artist-selector (artist) #'(lambda (cd) (equal artist (getf cd :artist))))
(defun select (selector-fn) (remove-if-not selector-fn *db*))

; let's define now generic selector

; keyword parameters
; (defun foo (&key a b c) (list a b c))
; (foo :a 1 :b 2 :c 3) => (1 2 3)

; default param
; (defun foo (&key a (b 20) c) (list a b c))

; supplied-p parameter - if we want to find out if we had a default param provided
; (defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
; c-p === bool for :c provided or not
; (foo :a 3) => (3 20 30 nil)
; (foo :a 3 :c 10) => (3 20 10 T)

(defun where (&key title artist rating (ripped nil ripped-p))
    #'(lambda (cd)
        (and
            (if title (equal (getf cd :title) title) t)
            (if artist (equal (getf cd :artist) artist) t)
            (if rating (equal (getf cd :rating) rating) t)
            (if ripped-p (equal (getf cd :ripped) ripped) t))))

; if condidtion then else
; if key provieded then eq else true

(select (where :rating 10 :artist "foobar"))

; (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
; (add-record (make-cd "Fly" "Crazy stuff" 1 nil))
; (add-record (make-cd "Low" "The piglets" 3 nil))
