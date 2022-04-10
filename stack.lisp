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
;;;;    Fox does not emphasize how a list can be used to implement a stack.
;;;;    见 SINGLY-LINKED-LIST-STACK, PERSISTENT-LIST-STACK
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
;;;    LINKED-LIST-STACK
;;;
;;;    Complications with layering data structures...
;;;    (setf *slls* (make-instance 'linked-list-stack :type 'integer))
;;;      Incompatible FILL-ELT type: NULL should be: INTEGER
;;;    (setf *slls* (make-instance 'linked-list-stack :type 'integer :fill-elt 0))
;;;      Invalid initialization argument:
;;;          :FILL-ELT
(defclass linked-list-stack (stack)
  ((list :initform (make-linked-list)))) ; FILL-ELT is never used!!

(defmethod size ((s linked-list-stack))
  (with-slots (list) s
    (size list)))

(defmethod emptyp ((s linked-list-stack))
  (with-slots (list) s
    (emptyp list)))

(defmethod clear ((s linked-list-stack))
  (with-slots (list) s
    (clear list)))

(defmethod push ((s linked-list-stack) obj)
  (with-slots (list) s
    (insert list 0 obj)))

(defmethod pop ((s linked-list-stack))
  (with-slots (list) s
    (delete list 0)))

(defmethod peek ((s linked-list-stack))
  (with-slots (list) s
    (nth list 0)))

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
;;;      - In particular, can't let COUNT be inconsistent with (length top)
;;;      - But inefficient to always calculate COUNT when PUSHing/POPping
;;;      - Initargs removed from slots
;;;      - Still possible to mangle an instance via WITH-SLOTS
;;;
;;;    (reduce #'(lambda (s elt) (push s elt)) '(2 4 6 8) :initial-value *ps*)
(defclass persistent-stack (stack)
  ((top :initform '())
   (count :initform 0 :type integer)))

(defmethod size ((s persistent-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s persistent-stack))
  (with-slots (top) s
    (null top)))

(defmethod clear ((s persistent-stack))
  (make-instance 'persistent-stack :type (type s)))

(flet ((initialize-stack (type top count)
         (let ((new-stack (make-instance 'persistent-stack :type type)))
           (with-slots ((new-top top) (new-count count)) new-stack
             (setf new-top top
                   new-count count))
           new-stack)))
  (defmethod push ((s persistent-stack) obj)
    (with-slots (type top count) s
      (initialize-stack type (cons obj top) (1+ count))))
  (defmethod pop ((s persistent-stack))
    (with-slots (type top count) s
      (values (initialize-stack type (rest top) (1- count)) (peek s)))) )

(defmethod peek ((s persistent-stack))
  (with-slots (top) s
    (first top)))

;;;
;;;    PERSISTENT-LIST-STACK
;;;
(let ((empty (make-persistent-list))) ; FILL-ELT is never used!!
  (defclass persistent-list-stack (stack)
    ((list :initform empty))))

(defmethod size ((s persistent-list-stack))
  (with-slots (list) s
    (size list)))

(defmethod emptyp ((s persistent-list-stack))
  (with-slots (list) s
    (emptyp list)))

(defmethod clear ((s persistent-list-stack))
  (make-instance 'persistent-list-stack :type (type s)))

(flet ((initialize-stack (type list)
         (let ((new-stack (make-instance 'persistent-list-stack :type type)))
           (with-slots ((new-list list)) new-stack
             (setf new-list list))
           new-stack)))
  (defmethod push ((s persistent-list-stack) obj)
    (with-slots (type list) s
      (initialize-stack type (insert list 0 obj))))
  (defmethod pop ((s persistent-list-stack))
    (with-slots (type list) s
      (values (initialize-stack type (delete list 0)) (peek s)))) )

(defmethod peek ((s persistent-list-stack))
  (with-slots (list) s
    (nth list 0)))
