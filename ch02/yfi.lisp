;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               yfi.lisp
;;;;
;;;;   Started:            Thu Jul 13 02:45:09 2023
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
;;;;   Notes: Interesting example of shared code--not superclass/subclass
;;;;          but rather 2 packages where one shares some of the other
;;;;          and "overrides" some. E.g., ADD names 2 distinct generic functions:
;;;;          YFI::ADD and YFI-KEYS::ADD. Even more interesting is YFI:=, which
;;;;          calls EQUALS. However, there are 2 such GFs: YFI::EQUALS and
;;;;          YFI-KEYS::EQUALS. The correct version is called in each package.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")

(defpackage :yfi
  (:use :common-lisp :core)
  (:export :yfi :length :inches :feet :yards :=)
  (:shadow :+ := :length))

(in-package :yfi)

(defclass yfi ()
  ((length :reader length :initarg :length)))

(defmethod initialize-instance :after ((yfi yfi) &rest initargs)
  (declare (ignore initargs))
  (assert (not (minusp (length yfi))) () "Length must be non-negative."))

(defun make-yfi (&rest args)
  (assert (every (every-pred #'integerp (compose #'not #'minusp)) args)
          ()
          "Length components must be non-negative integers.")
  (flet ((feet->inches (feet)
           (* feet 12))
         (yards->inches (yards)
           (* yards 36)))
    (if (null args)
        (error "SRSLY?")
        (destructuring-bind (x . more) args
          (if (null more)
              (make-instance 'yfi :length x)
              (destructuring-bind (y . more) more
                (if (null more)
                    (make-instance 'yfi :length (cl:+ (feet->inches x) y))
                    (destructuring-bind (z . more) more
                      (if (null more)
                          (make-instance 'yfi :length (cl:+ (yards->inches x) (feet->inches y) z))
                          (error "Invalid initialization: ~A" args)))) )))) ))

(defgeneric inches (yfi)
  (:documentation "Return the number of inches left over in this YFI after yards/feet have been accounted for."))
(defmethod inches ((yfi yfi))
  (mod (length yfi) 12))

(defgeneric feet (yfi)
  (:documentation "Return the number of feet remaining in this YFI after yards have been accounted for."))
(defmethod feet ((yfi yfi))
  (mod (floor (length yfi) 12) 3))

(defgeneric yards (yfi)
  (:documentation "Return the number of yards represented by this YFI."))
(defmethod yards ((yfi yfi))
  (values (floor (length yfi) 36)))

(defmethod print-object ((yfi yfi) stream)
  (print-unreadable-object (yfi stream :type t)
    (format stream "yards: ~D feet: ~D inches: ~D" (yards yfi) (feet yfi) (inches yfi))))

(defgeneric add (yfi obj)
  (:documentation "Create a new YFI of length OBJ and the given YFI."))
(defmethod add ((yfi1 yfi) (yfi2 yfi))
  (make-yfi (cl:+ (length yfi1) (length yfi2))))
(defmethod add ((yfi yfi) (n integer))
  (assert (>= n 0) () "N must be non-negative.")
  ;;  (make-yfi (cl:+ (length yfi) n)))
  (add yfi (make-yfi n)))
(defmethod add ((n integer) (yfi yfi))
  (add yfi n))
(defmethod add ((m integer) (n integer))
  (add (make-yfi m) (make-yfi n)))

;; (defun + (&rest objs)
;;   (if (null objs)
;;       (make-yfi 0)
;;       (destructuring-bind (obj . more) objs
;;         (reduce #'add more :initial-value (make-yfi obj)))) )

(let ((zero (make-yfi 0)))
  (defun + (&rest objs)
    (reduce #'add objs :initial-value zero)))

(defgeneric equals (yfi obj)
  (:documentation "Are the YFI and OBJ of the same length?"))
(defmethod equals ((yfi1 yfi) (yfi2 yfi))
  (cl:= (length yfi1) (length yfi2)))
(defmethod equals ((yfi yfi) (n integer))
  ;; (cl:= (length yfi) n))
  (equals yfi (make-yfi n)))
(defmethod equals ((n integer) (yfi yfi))
  (equals yfi n))
(defmethod equals ((m integer) (n integer))
  ;; (cl:= m n))
  (equals (make-yfi m) (make-yfi n)))

(defun = (obj &rest objs)
  (every #'(lambda (elt) (equals obj elt)) objs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :yfi-keys
  (:use :common-lisp :core :yfi)
  (:shadowing-import-from :yfi :length :=)
  (:shadow :+))

(in-package :yfi-keys)

(defun make-yfi (&key (inches 0) (feet 0) (yards 0))
  (assert (every (every-pred #'integerp (compose #'not #'minusp))
                 (list yards feet inches))
          ()
          "Length components must be non-negative integers.")
  (flet ((feet->inches (feet)
           (* feet 12))
         (yards->inches (yards)
           (* yards 36)))
    (make-instance 'yfi :length (cl:+ (yards->inches yards) (feet->inches feet) inches))))

(defgeneric add (yfi obj)
  (:documentation "Create a new YFI of length OBJ and the given YFI."))
(defmethod add ((yfi1 yfi) (yfi2 yfi))
  (make-instance 'yfi :length (cl:+ (length yfi1) (length yfi2))))
(defmethod add ((yfi yfi) (n integer))
  (assert (>= n 0) () "N must be non-negative.")
  (add yfi (make-instance 'yfi :length n)))
(defmethod add ((n integer) (yfi yfi))
  (add yfi n))
(defmethod add ((m integer) (n integer))
  (add (make-instance 'yfi :length m) (make-instance 'yfi :length n)))

(let ((zero (make-yfi)))
  (defun + (&rest objs)
    (reduce #'add objs :initial-value zero)))

(defgeneric equals (yfi obj)
  (:documentation "Are the YFI and OBJ of the same length?"))
(defmethod equals ((yfi1 yfi) (yfi2 yfi))
  (cl:= (length yfi1) (length yfi2)))
(defmethod equals ((yfi yfi) (n integer))
  (equals yfi (make-instance 'yfi :length n)))
(defmethod equals ((n integer) (yfi yfi))
  (equals yfi n))
(defmethod equals ((m integer) (n integer))
  (equals (make-instance 'yfi :length m) (make-instance 'yfi :length n)))
