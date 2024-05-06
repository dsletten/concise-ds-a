;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               yfi2.lisp
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
;;;;   Notes: Overly ambitious design that tried to allow interplay between YFI instances and integers.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")

(defpackage :yfi2
  (:use :common-lisp :core)
  (:export :yfi :make-yfi :length :inches :feet :yards := :+)
  (:shadow :+ := :length))

(in-package :yfi2)

(defclass yfi ()
  ((length :reader length :initarg :length :initform 0)))

(defmethod initialize-instance :after ((yfi yfi) &rest initargs)
  (declare (ignore initargs))
  (assert (not (minusp (length yfi))) () "Length must be non-negative."))

(defun make-yfi (&rest args)
  (assert (every (every-pred #'integerp (complement #'minusp)) args)
          ()
          "Length components must be non-negative integers.")
  (flet ((feet->inches (feet)
           (* feet 12))
         (yards->inches (yards)
           (* yards 36)))
    (if (null args)
        (make-instance 'yfi)
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

;; (defun make-yfi (&optional (x nil) (y nil) (z nil))
;;   (cond ((null x) (make-yfi 0 0 0))
;;         ((null y) (make-yfi 0 0 x))
;;         ((null z) (make-yfi 0 x y))
;;         (t (assert (every (every-pred #'integerp (complement #'minusp)) (list x y z))
;;                    ()
;;                    "Length components must be non-negative integers.")
;;            (flet ((feet->inches (feet)
;;                     (* feet 12))
;;                   (yards->inches (yards)
;;                     (* yards 36)))
;;              (make-instance 'yfi :length (cl:+ (yards->inches x) (feet->inches y) z)))) ))

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

(defmethod length ((n integer)) n)

(defun add (yfi1 yfi2)
  (make-instance 'yfi :length (cl:+ (length yfi1) (length yfi2))))

(let ((zero (make-yfi 0)))
  (defun + (&rest objs)
    (reduce #'add objs :initial-value zero)))

(defun equals (yfi1 yfi2)
  (cl:= (length yfi1) (length yfi2)))

(defun = (obj &rest objs)
  (every #'(lambda (elt) (equals obj elt)) objs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :yfi-keys2
  (:use :common-lisp :core :yfi)
  (:shadowing-import-from :yfi :length := :+)
  (:shadow :make-yfi))

(in-package :yfi-keys2)

(defun make-yfi (&key (inches 0) (feet 0) (yards 0))
  (yfi:make-yfi yards feet inches))
