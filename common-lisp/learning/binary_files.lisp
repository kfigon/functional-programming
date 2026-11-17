; big endian
(defun read-u8 (in) (ldb (byte 8 0) (read-byte in)))

(defun read-u16 (in) 
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

; string encoding approaches
; fixed len
; var len, but at the end of struct - just read till end
; delimiter - read till nil
; start string with int that means how many bytes to read

(defconstant +null-char+ (code-char 0))

(defun read-null-terminated-ascii (in)
    (with-output-to-string (s) ; building string char by char
        (loop for c = (code-char (read-byte in))
            until (char= c +null-char+)
            do (write-char c s))))


; binary format used in mp3 files
(defclass id3-tag ()
    ((identifier    :initarg :identifier    :accessor identifier)
     (major-version :initarg :major-version :accessor major-version)
     (revision      :initarg :revision      :accessor revision)
     (flags         :initarg :flags         :accessor flags)
     (size          :initarg :size          :accessor size)
     (frames        :initarg :frames        :accessor frames)))

(defun read-id3-tag (in)
    (let ((tag (make-instance 'id3-tag)))
        (with-slots (identifier major-version revision flags size frames)
            (setf identifier    (read-iso-885901-string in :len 3))
            (setf major-version (read-u1 in))
            (setf revision      (read-u1 in))
            (setf flags         (read-u1 in))
            (setf size          (read-id3-encoded-size in))
            (setf frames        (read-id3-frames in :tag-size size)))
        tag))

; write all functions, mostly same, but lot of boilerplate
; description took 4 lines, code is already 40 and not finished.

;----------------- lisp way!
; 