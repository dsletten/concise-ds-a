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
;;;;   Modulus should be > 1?!
;;;; * (setf *cc* (make-counter 1))
;;;; #<CYCLIC-COUNTER 0/1>
;;;; * (advance *cc*)
;;;; 0
;;;; * (advance *cc*)
;;;; 0
;;;; * (advance *cc*)
;;;; 0
;;;;
;;;;    http://xahlee.info/comp/unicode_punctuation_symbols.html
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")

(defpackage :cyclic-counter (:use :common-lisp :core) (:shadow :set))

(in-package :cyclic-counter)

;;;
;;;    SBCL enforce slot :TYPE
;;;    
(declaim (optimize safety))

(defclass counter () ())

;;;
;;;    Still not sure if this right?!
;;;    
(defmethod make-instance :around ((class (eql (find-class 'counter))) &rest initargs)
  (declare (ignore class initargs))           
  (error "Cannot instantiate COUNTER."))

(defgeneric index (counter)
  (:documentation "Return the INDEX for COUNTER."))
(defmethod index ((c counter))
  (declare (ignore c))
  (error "COUNTER does not implement INDEX"))

(defgeneric modulus (counter)
  (:documentation "Return the MODULUS for COUNTER."))
(defmethod modulus ((c counter))
  (declare (ignore c))
  (error "COUNTER does not implement MODULUS"))

(defgeneric advance (counter &optional n)
  (:documentation "Advance the COUNTER by N."))
(defmethod advance ((c counter) &optional n)
  (declare (ignore c n))
  (error "COUNTER does not implement ADVANCE"))

(defgeneric set (counter n)
  (:documentation "Set the COUNTER to N."))
(defmethod set ((c counter) n)
  (declare (ignore c n))
  (error "COUNTER does not implement SET"))

(defgeneric reset (counter)
  (:documentation "Set the COUNTER to 0."))
(defmethod reset ((c counter))
  (set c 0))

(defmethod print-object ((c counter) stream)
  (format stream "#⧙~A (~D ~D)⧘" (class-name (class-of c)) (index c) (modulus c)))
 
;(set-dispatch-macro-character #\# #\GREEK_SMALL_LETTER_LAMDA ; !!
(set-dispatch-macro-character #\# #\⧙
  #'(lambda (stream ch arg)
      (declare (ignore ch arg))
      (destructuring-bind (class (index modulus)) (read-delimited-list #\⧘ stream t)
        (ecase class
          (cyclic-counter (let ((counter (make-counter modulus)))
                            (advance counter index)
                            counter))
          (persistent-cyclic-counter (advance (make-persistent-counter modulus) index)))) ))

(set-syntax-from-char #\⧘ #\))

(defclass cyclic-counter (counter)
  ((index :initform 0 :reader index :type (integer 0))
   (modulus :initform 1 :initarg :modulus :reader modulus :type (integer 1))))

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

(defmethod set ((counter cyclic-counter) n)
  (with-slots (index modulus) counter
    (setf index (mod n modulus))))

(defclass persistent-cyclic-counter (counter)
  ((index :initform 0 :initarg :index :reader index :type (integer 0))
   (modulus :initform 1 :initarg :modulus :reader modulus :type (integer 1))))

;;;
;;;    INDEX can only be specified if MODULUS is too. Otherwise no way to normalize.
;;;    
(defmethod initialize-instance :around ((c persistent-cyclic-counter) &rest initargs)
  (let ((index (getf initargs :index))
        (modulus (getf initargs :modulus)))
    (cond ((null index) (call-next-method))
          ((null modulus) (error "Inconsistent initialization. Cannot rely on default MODULUS with specified INDEX."))
          (t (assert (>= modulus 1) () "Modulus must be at least 1.")
             (call-next-method c :index (mod index modulus) :modulus modulus)))) )

(defun make-persistent-counter (m)
  (make-instance 'persistent-cyclic-counter :modulus m))

(defmethod advance ((counter persistent-cyclic-counter) &optional (n 1))
  (make-instance 'persistent-cyclic-counter :index (+ (index counter) n) :modulus (modulus counter)))

(defmethod set ((counter persistent-cyclic-counter) n)
  (make-instance 'persistent-cyclic-counter :index n :modulus (modulus counter)))
