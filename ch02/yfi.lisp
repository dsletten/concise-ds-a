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
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :yfi (:use :common-lisp :lang :test) (:shadow :+ := :length))

(in-package :yfi)

(defclass yfi ()
  ((length :reader length :initarg :length)))

(defun make-yfi (&rest args)
  (flet ((feet->inches (feet)
           (* feet 12))
         (yards->inches (yards)
           (* yards 36)))
    (destructuring-bind (x . more) args
      (if (null more)
          (etypecase x
            (yfi x)
            (integer (make-instance 'yfi :length x)))
          (destructuring-bind (y . more) more
            (if (null more)
                (make-instance 'yfi :length (cl:+ (feet->inches x) y))
                (destructuring-bind (z . more) more
                  (if (null more)
                      (make-instance 'yfi :length (cl:+ (yards->inches x) (feet->inches y) z))
                      (error "Invalid initialization: ~A" args)))) )))) )

(defmethod print-object ((yfi yfi) stream)
  (print-unreadable-object (yfi stream :type t)
    (format stream "yards: ~D feet: ~D inches: ~D" (yards yfi) (feet yfi) (inches yfi))))

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
  (floor (length yfi) 36))

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

(defun + (&rest objs)
  (make-yfi (reduce #'add objs :initial-value (make-yfi 0))))

(defgeneric equals (yfi obj)
  (:documentation "Are the YFI and OBJ of the same length?"))
(defmethod equals ((yfi1 yfi) (yfi2 yfi))
  (cl:= (length yfi1) (length yfi2)))
(defmethod equals ((yfi yfi) (n integer))
  (cl:= (length yfi) n))
(defmethod equals ((n integer) (yfi yfi))
  (equals yfi n))
(defmethod equals ((m integer) (n integer))
  (cl:= m n))

(defun = (obj &rest objs)
  (every #'(lambda (elt) (equals obj elt)) objs))

(deftest test-length ()
  (check
   (cl:= 0 (length (make-yfi 0)))
   (cl:= 1 (length (make-yfi 1)))
   (cl:= 12 (length (make-yfi 1 0)))
   (cl:= 36 (length (make-yfi 1 0 0)))
   (cl:= 49 (length (make-yfi 1 1 1)))
   (cl:= 100 (length (+ (make-yfi 49) (make-yfi 51))))
   (cl:= 100 (length (+ (make-yfi 49) 51)))
   (cl:= 100 (length (+ 49 (make-yfi 51))))
   (cl:= 100 (length (+ 49 51)))) )

(deftest test-inches ()
  (check
   (cl:= 0 (inches (make-yfi 0)))
   (cl:= 1 (inches (make-yfi 1)))
   (cl:= 0 (inches (make-yfi 1 0)))
   (cl:= 0 (inches (make-yfi 1 0 0)))
   (cl:= 1 (inches (make-yfi 1 1 1)))
   (cl:= 0 (inches (+ (make-yfi 8) (make-yfi 4)))) ))

(deftest test-feet ()
  (check
   (cl:= 0 (feet (make-yfi 0)))
   (cl:= 0 (feet (make-yfi 1)))
   (cl:= 1 (feet (make-yfi 1 0)))
   (cl:= 0 (feet (make-yfi 1 0 0)))
   (cl:= 1 (feet (make-yfi 1 1 1)))
   (cl:= 0 (feet (+ (make-yfi 16) (make-yfi 20)))) ))

(deftest test-yards ()
  (check
   (cl:= 0 (yards (make-yfi 0)))
   (cl:= 0 (yards (make-yfi 1)))
   (cl:= 0 (yards (make-yfi 1 0)))
   (cl:= 1 (yards (make-yfi 1 0 0)))
   (cl:= 1 (yards (make-yfi 1 1 1)))
   (cl:= 1 (yards (+ (make-yfi 12) (make-yfi 12) (make-yfi 12)))) ))

(deftest test-+ ()
  (check
   (= 0 (+))
   (= 1 (+ 0 1))
   (= 1 (+ (make-yfi 0) (make-yfi 1)))
   (= (+ (make-yfi 20) (make-yfi 30)) (+ (make-yfi 30) (make-yfi 20)))
   (= (+ (make-yfi 1 2 3) (make-yfi 4 5 6)) (+ (make-yfi 4 5 6) (make-yfi 1 2 3)))
   (= (loop for i from 1 to 10 summing i)
      (apply #'+ (loop for i from 1 to 10 collect i))
      (apply #'+ (mapcar #'make-yfi (loop for i from 1 to 10 collect i)))) ))

   
