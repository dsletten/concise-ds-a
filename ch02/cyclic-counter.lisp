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
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")

(defpackage :cyclic-counter (:use :common-lisp :core) (:shadow :set))

(in-package :cyclic-counter)

(defclass counter () ())

(defgeneric index (counter)
  (:documentation "Return the INDEX for COUNTER."))

(defgeneric modulus (counter)
  (:documentation "Return the MODULUS for COUNTER."))

(defgeneric advance (counter &optional n)
  (:documentation "Advance the COUNTER by N."))

(defgeneric set (counter n)
  (:documentation "Set the COUNTER to N."))

(defgeneric reset (counter)
  (:documentation "Set the COUNTER to 0."))
(defmethod reset ((c counter))
  (set c 0))

;; (defmethod print-object ((c counter) stream)
;;   (print-unreadable-object (c stream :type t)
;;     (format stream "~D/~D" (index c) (modulus c))))

(defmethod print-object ((c counter) stream)
  (format stream "#λ~A ~D/~Dλ" (type-of c) (index c) (modulus c)))

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

(defmethod set ((counter cyclic-counter) n)
  (with-slots (index modulus) counter
    (setf index (mod n modulus))))

;; (defun advance (counter &optional (n 1))
;;   "Advance the COUNTER by N."
;;   (with-slots (index modulus) counter
;;     (setf index (mod (+ index n) modulus))))

;; (defun set (counter &optional (n 0))
;;   "Set the COUNTER to the value N."
;;   (with-slots (index modulus) counter
;;     (setf index (mod n modulus))))

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

;; (defmethod initialize-instance :after ((c persistent-cyclic-counter) &rest initargs)
;;   (declare (ignore initargs))           
;;   (with-slots (index modulus) c
;;     (assert (>= modulus 1) () "Modulus must be at least 1.")
;;     (setf index (mod index modulus))))

;;;
;;;    This :BEFORE method is actually inconsistent with having an :INITFORM on
;;;    the MODULUS slot. This method assumes that the :MODULUS initarg will always
;;;    be supplied.
;;;    
(defmethod initialize-instance :before ((c persistent-cyclic-counter) &rest initargs)
  (assert (>= (getf initargs :modulus) 1) () "Modulus must be at least 1."))

(defmethod initialize-instance :after ((c persistent-cyclic-counter) &rest initargs)
  (declare (ignore initargs))           
  (with-slots (index modulus) c
    (setf index (mod index modulus))))

;; (defun make-persistent-counter (m &optional n)
;;   (cond ((null n) (make-persistent-counter 0 m))
;;         ((< n 1) (error "Modulus must be at least 1."))
;;         (t (make-instance 'persistent-cyclic-counter :index (mod m n) :modulus n))))

;;;
;;;    Weird way to handle OPTIONAL arg?
;;;    What is the point of this function? User should not be able to create a counter
;;;    with a non-zero index? This is only ever used by ADVANCE and SET, but they always
;;;    provide both slot values...
;;;    
;; (defun make-persistent-counter (m &optional n)
;;   (if (null n)
;;       (make-instance 'persistent-cyclic-counter :modulus m)
;;       (make-instance 'persistent-cyclic-counter :index m :modulus n))) ; ????

(defun make-persistent-counter (m)
  (make-instance 'persistent-cyclic-counter :modulus m))

(defmethod advance ((counter persistent-cyclic-counter) &optional (n 1))
  (make-instance 'persistent-cyclic-counter :modulus (modulus counter) :index (+ (index counter) n)))

(defmethod set ((counter persistent-cyclic-counter) n)
  (make-instance 'persistent-cyclic-counter :modulus (modulus counter) :index n))

;; (defmethod advance ((counter persistent-cyclic-counter) &optional (n 1))
;;   (make-persistent-counter (+ (index counter) n) (modulus counter)))

;; (defmethod set ((counter persistent-cyclic-counter) n)
;;   (make-persistent-counter n (modulus counter)))


(set-dispatch-macro-character #\# #\GREEK_SMALL_LETTER_LAMDA ; !!
  #'(lambda (stream ch arg)
      (declare (ignore ch arg))
      (destructuring-bind (class fraction) (read-delimited-list #\GREEK_SMALL_LETTER_LAMDA stream t)
(print fraction)
        (let ((index (numerator fraction))
              (modulus (denominator fraction)))
          (print index)
          (print modulus)
          (ecase class
            (cyclic-counter (let ((counter (make-counter modulus)))
                              (advance counter index)
                              counter))
            (persistent-cyclic-counter (advance (make-persistent-counter modulus) index)))) )))
;      `(make-set :test #'equalp :elements (list ,@(read-delimited-list #\} stream t)))) ) ; Should this be EQUALP?
;;   #'(lambda (stream ch arg)
;;       (declare (ignore ch arg))
;;       (let* ((class (read stream))
;;              (fraction (read stream))
;;              (index (numerator fraction))
;;              (modulus (denominator fraction)))
;;         (ecase class
;;           (cyclic-counter (let ((counter (make-counter modulus)))
;;                             (advance counter index)
;;                             counter))
;;           (persistent-cyclic-counter (advance (make-persistent-counter modulus) index)))) ))
;; ;      `(make-set :test #'equalp :elements (list ,@(read-delimited-list #\} stream t)))) ) ; Should this be EQUALP?

(set-syntax-from-char #\GREEK_SMALL_LETTER_LAMDA #\))


;; (set-dispatch-macro-character #\# #\[
;;   #'(lambda (stream ch arg)
;;       (declare (ignore ch arg))
;;       (destructuring-bind (m &optional n step) (read-delimited-list #\] stream t)
;;         (if step
;;             (if (and (numberp step)
;;                      (or (and (numberp m) (numberp n))
;;                          (and (characterp m) (characterp n))))
;;                 `',(make-range m n step)
;;                 `(make-range ,m ,n ,step))
;;             (if n
;;                 (if (or (and (numberp m) (numberp n))
;;                         (and (characterp m) (characterp n)))
;;                     `',(make-range m n)
;;                     `(make-range ,m ,n))
;;                 (if (or (numberp m) (characterp m))
;;                     `',(make-range m)
;;                     `(make-range ,m)))) )))

;;;
;;;    D'oh!
;;;    
;; (set-macro-character #\" #'(lambda (stream ch)
;;                              (declare (ignore ch))
;;                              `(vector ,@(read-delimited-list #\" stream t))))
;; (set-syntax-from-char #\" #\))
