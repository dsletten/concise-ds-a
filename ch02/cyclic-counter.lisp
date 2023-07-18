;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               cyclic-counter.lisp
;;;;
;;;;   Started:            Wed Jun 21 01:13:31 2023
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
;;;;   Notes: Java Version of Fox's book.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :cyclic-counter (:use :common-lisp :lang :test) (:shadow :set))

(in-package :cyclic-counter)

(defclass counter () ())

(defgeneric index (counter)
  (:documentation "Return the INDEX for COUNTER."))

(defgeneric modulus (counter)
  (:documentation "Return the MODULUS for COUNTER."))

(defgeneric advance (counter &optional n)
  (:documentation "Advance the COUNTER by N."))

(defgeneric initialize (counter &optional n)
  (:documentation "Reset the COUNTER to N."))

(defmethod print-object ((c counter) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~D/~D" (index c) (modulus c))))

(defclass cyclic-counter (counter)
  ((index :initform 0 :reader index)
   (modulus :initform 1 :initarg :modulus :reader modulus)))

(defmethod initialize-instance :after ((c cyclic-counter) &rest initargs)
  (declare (ignore initargs))           
  (assert (>= (modulus c) 1) () "Modulus must be at least 1."))

(defun make-counter (n)
  (make-instance 'cyclic-counter :modulus n))

;;;
;;;    The modular arithmetic must be located on these methods since the
;;;    mutable counters don't rely on INITIALIZE-INSTANCE after creation.
;;;    
(defmethod advance ((counter cyclic-counter) &optional (n 1))
  (with-slots (index modulus) counter
    (setf index (mod (+ index n) modulus))))

(defmethod initialize ((counter cyclic-counter) &optional (n 0))
  (with-slots (index modulus) counter
    (setf index (mod n modulus))))

;; (defun initialize (counter &optional (n 0))
;;   "Initialize the COUNTER to the value N."
;;   (with-slots (index modulus) counter
;;     (setf index (mod n modulus))))

;; (defun advance (counter &optional (n 1))
;;   "Advance the COUNTER by N."
;;   (with-slots (index modulus) counter
;;     (setf index (mod (+ index n) modulus))))

(deftest test-make-counter ()
  (check
   (handler-case (make-counter 0)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with modulus of 1.~%")))) )

(deftest test-counter ()
  (check
   (let ((c (make-counter 10)))
     (loop repeat 11
           do (advance c))
     (= 1 (index c)))))

(defclass persistent-cyclic-counter (counter)
  ((index :initform 0 :initarg :index :reader index)
   (modulus :initform 1 :initarg :modulus :reader modulus)))

;;;
;;;    It seems cleaner to have the constructor (MAKE-PERSISTENT-COUNTER) pass in the
;;;    validated args rather than have an :AFTER method enforce the rules and possibly
;;;    modify existing slots. However, there is no way to restrict creation of counters
;;;    to the constructor (e.g., make MAKE-INSTANCE private...), so this is the only
;;;    way to ensure that every instance is in a legal state.
;;;
;;;    This is peculiar to CLOS since the "constructor" is sort of split between MAKE-PERSISTENT-COUNTER
;;;    and INITIALIZE-INSTANCE.
;;;    
(defmethod initialize-instance :after ((c persistent-cyclic-counter) &rest initargs)
  (declare (ignore initargs))           
  (with-slots (index modulus) c
    (assert (>= modulus 1) () "Modulus must be at least 1.")
    (setf index (mod index modulus))))

;;;
;;;    Weird way to handle OPTIONAL arg?
;;;    
(defun make-persistent-counter (m &optional n)
  (if (null n)
      (make-instance 'persistent-cyclic-counter :modulus m)
      (make-instance 'persistent-cyclic-counter :index m :modulus n))) ; ????

;; (defun make-persistent-counter (m &optional n)
;;   (cond ((null n) (make-persistent-counter 0 m))
;;         ((< n 1) (error "Modulus must be at least 1."))
;;         (t (make-instance 'persistent-cyclic-counter :index (mod m n) :modulus n))))

(defmethod advance ((counter persistent-cyclic-counter) &optional (n 1))
  (make-persistent-counter (+ (index counter) n) (modulus counter)))

(defmethod initialize ((counter cyclic-counter) &optional (n 0))
  (make-persistent-counter n (modulus counter)))

(deftest test-make-persistent-counter ()
  (check
   (handler-case (make-persistent-counter 0)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with modulus of 1.~%")))) )

(deftest test-persistent-counter ()
  (check
   (loop repeat 11
         for c = (make-persistent-counter 10) then (advance c)
         finally (return (= 1 (index (advance c)))) )))


