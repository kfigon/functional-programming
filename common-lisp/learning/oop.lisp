; class fields are called slots here
(defclass person ()
    ((name
        :initarg :name ; key in the constructor
        :accessor name ; getter and setter
        :initform (error "name is required")) ; validation or default param
     (age
        :initarg :age
        :reader get-age
        :initform 0))) 

; we can also define constructors when we can access all parameters
; (defmethod initialize-instance 'person)

(print (make-instance 'person :name "John" :age 20))
(print (name (make-instance 'person :name "John" :age 20)))

; interface definition, virtual method
(defgeneric fetch (db))

(defclass prod-db () ())
(defmethod fetch ((d prod-db)) "real prod value")

(defclass mock-db () ())
(defmethod fetch ((d mock-db)) "mocking some value")

(print (fetch (make-instance 'prod-db)))
(print (fetch (make-instance 'mock-db)))


; we can define hooks around method calls 
; we can define method specializations
; we can define multiple dispatch methods, specializing in various
; types in the hierarchy (first method with class A and B as args, second method with both B etc)
(defclass cat () ())
(defclass dog () ())
(defgeneric interact (a b))

(defmethod interact ((a dog) (b cat))
  (format t "Dog chases cat.~%"))

(defmethod interact ((a cat) (b dog))
  (format t "Cat escapes dog.~%"))

(defmethod interact ((a dog) (b dog))
  (format t "Dogs play together.~%"))