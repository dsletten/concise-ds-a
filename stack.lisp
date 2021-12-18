;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               stack.lisp
;;;;
;;;;   Started:            Sat Nov 13 14:14:10 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(in-package :containers)

;;;
;;;    STACK
;;;    
(defclass stack (dispenser)
  ()
  (:documentation "A stack is a dispenser that holds a sequence of elements that can be accessed, inserted, or removed at only one end, the top (LIFO)."))

(defgeneric push (stack obj)
  (:documentation "Push an object onto the top of the stack"))
(defmethod push :around ((s stack) obj)
  (if (typep obj (type s))
      (call-next-method)
      (error "~A is not of type ~A" obj (type s))))
(defmethod push ((s stack) obj)
  (declare (ignore s obj))
  (error "STACK does not implement PUSH"))

;;;
;;;    POP should ensure that the reference to the object just removed from the stack has been released
;;;    to allow that object to be GC'd as eligible.
;;;    
(defgeneric pop (stack)
  (:documentation "Pop an object from the top of the stack"))
(defmethod pop :around ((s stack))
  (if (emptyp s)
      (error "Stack is empty")
      (call-next-method)))
(defmethod pop ((s stack))
  (declare (ignore s))
  (error "STACK does not implement POP"))

(defgeneric peek (stack)
  (:documentation "Examine object on the top of the stack"))
(defmethod peek :around ((s stack))
  (if (emptyp s)
      (error "Stack is empty")
      (call-next-method)))
(defmethod peek ((s stack))
  (declare (ignore s))
  (error "STACK does not implement PEEK"))

;;;
;;;    ARRAY-STACK
;;;    
(defclass array-stack (stack)
  ((store)))

(defmethod initialize-instance :after ((s array-stack) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) s
    (setf store (make-array 20 :adjustable t :fill-pointer 0 :element-type (type s)))) )

(defmethod size ((s array-stack))
  (with-slots (store) s
    (length store)))

(defmethod emptyp ((s array-stack))
  (zerop (size s)))

(defmethod clear ((s array-stack))
  (with-slots (store) s
    (setf (fill-pointer store) 0)))

(defmethod push ((s array-stack) obj)
  (with-slots (store) s
    (vector-push-extend obj store)))

(defmethod pop ((s array-stack))
  (with-slots (store) s
    (vector-pop store)))

(defmethod peek ((s array-stack))
  (with-slots (store) s
    (aref store (1- (size s)))) )

;;;
;;;    LINKED-STACK
;;;    
(defclass linked-stack (stack)
  ((top :initform '())
   (count :initform 0)))

(defmethod size ((s linked-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s linked-stack))
  (with-slots (top) s
    (null top)))

(defmethod clear ((s linked-stack))
  (with-slots (top count) s
    (setf top '()
          count 0)))

(defmethod push ((s linked-stack) obj)
  (with-slots (top count) s
;    (cl:push obj top)  ; Duh!
    (setf top (cons obj top))
    (incf count)))

(defmethod pop ((s linked-stack))
  (with-slots (top count) s
;    (cl:pop top)))
;    (prog1 (first top)
    (prog1 (peek s)
      (setf top (rest top))
      (decf count))))

(defmethod peek ((s linked-stack))
  (with-slots (top) s
    (first top)))

;;;
;;;    HASH-TABLE-STACK
;;;
(defclass hash-table-stack (stack)
  ((store :initform (make-hash-table))))

(defmethod size ((s hash-table-stack))
  (with-slots (store) s
    (hash-table-count store)))

(defmethod emptyp ((s hash-table-stack))
  (zerop (size s)))

(defmethod clear ((s hash-table-stack))
  (with-slots (store) s
    (clrhash store)))

(defmethod push ((s hash-table-stack) obj)
  (with-slots (store) s
    (setf (gethash (1+ (size s)) store) obj)))

(defmethod pop ((s hash-table-stack))
  (with-slots (store) s
;    (prog1 (gethash (size s) store)
    (prog1 (peek s)
      (remhash (size s) store))))

(defmethod peek ((s hash-table-stack))
  (with-slots (store) s
    (values (gethash (size s) store))))

;;;
;;;    PERSISTENT-STACK (Linked stack)
;;;    - This could be a subclass of LINKED-STACK and simply override CLEAR/PUSH/POP
;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-STACK...
;;;
;;;
;;;    (reduce #'(lambda (s elt) (push s elt)) '(2 4 6 8) :initial-value *ps*)
(defclass persistent-stack (stack)
  ((top :initform '() :initarg :top)
   (count :initform 0 :initarg :count))) ; Should not rely on arg?? Consistent with (length top)???

(defmethod size ((s persistent-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s persistent-stack))
  (with-slots (top) s
    (null top)))

(defmethod clear ((s persistent-stack))
  (make-instance 'persistent-stack :type (type s)))

(defmethod push ((s persistent-stack) obj)
  (with-slots (type top count) s
    (make-instance 'persistent-stack :type type :top (cons obj top) :count (1+ count))))

(defmethod pop ((s persistent-stack))
  (with-slots (type top count) s
    (values (make-instance 'persistent-stack :type type :top (rest top) :count (1- count)) (peek s))))

(defmethod peek ((s persistent-stack))
  (with-slots (top) s
    (first top)))
