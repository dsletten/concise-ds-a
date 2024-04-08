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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :yfi
  (:use :common-lisp :lang :test)
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
  (values (floor (length yfi) 36)))

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
  (reduce #'add objs :initial-value (make-yfi 0)))

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

(deftest test-make-yfi ()
  (check
   (handler-case (make-yfi 1 -2 3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Length components must be non-negative integers.")))) )

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
   (typep (+) 'yfi)
   (typep (+ 1) 'yfi)
   (= 0 (+))
   (= 1 (+ 0 1))
   (= 1 (+ (make-yfi 0) (make-yfi 1)))
   (= (+ (make-yfi 20) (make-yfi 30)) (+ (make-yfi 30) (make-yfi 20)))
   (= (+ (make-yfi 1 2 3) (make-yfi 4 5 6)) (+ (make-yfi 4 5 6) (make-yfi 1 2 3)))
   (= (loop for i from 1 to 10 summing i)
      (apply #'+ (loop for i from 1 to 10 collect i))
      (apply #'+ (mapcar #'make-yfi (loop for i from 1 to 10 collect i)))) ))

(deftest test-= ()
  (check
   (= 0)
   (= 1 1)
   (not (= 0 1))
   (= (make-yfi 5) (+ 2 3) (+ (make-yfi 2) (make-yfi 3)))) )
   
(defpackage :yfi-keys
  (:use :common-lisp :lang :yfi :test)
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

(defun + (&rest objs)
  (reduce #'add objs :initial-value (make-instance 'yfi :length 0)))

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

(deftest test-make-yfi ()
  (check
   (handler-case (make-yfi :yards 1 :feet -2 :inches 3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Length components must be non-negative integers.")))) )

(deftest test-length ()
  (check
   (cl:= 0 (length (make-yfi)))
   (cl:= 1 (length (make-yfi :inches 1)))
   (cl:= 12 (length (make-yfi :feet 1)))
   (cl:= 36 (length (make-yfi :yards 1)))
   (cl:= 49 (length (make-yfi :yards 1 :feet 1 :inches 1)))
   (cl:= 100 (length (+ (make-yfi :inches 49) (make-yfi :inches 51))))
   (cl:= 100 (length (+ (make-yfi :inches 49) 51)))
   (cl:= 100 (length (+ 49 (make-yfi :inches 51))))
   (cl:= 100 (length (+ 49 51)))) )

(deftest test-inches ()
  (check
   (cl:= 0 (inches (make-yfi)))
   (cl:= 1 (inches (make-yfi :inches 1)))
   (cl:= 0 (inches (make-yfi :feet 1)))
   (cl:= 0 (inches (make-yfi :yards 1)))
   (cl:= 1 (inches (make-yfi :yards 1 :feet 1 :inches 1)))
   (cl:= 0 (inches (+ (make-yfi :inches 8) (make-yfi :inches 4)))) ))

(deftest test-feet ()
  (check
   (cl:= 0 (feet (make-yfi)))
   (cl:= 0 (feet (make-yfi :inches 1)))
   (cl:= 1 (feet (make-yfi :feet 1)))
   (cl:= 0 (feet (make-yfi :yards 1)))
   (cl:= 1 (feet (make-yfi :yards 1 :feet 1 :inches 1)))
   (cl:= 0 (feet (+ (make-yfi :inches 16) (make-yfi :inches 20)))) ))

(deftest test-yards ()
  (check
   (cl:= 0 (yards (make-yfi)))
   (cl:= 0 (yards (make-yfi :inches 1)))
   (cl:= 0 (yards (make-yfi :feet 1)))
   (cl:= 1 (yards (make-yfi :yards 1)))
   (cl:= 1 (yards (make-yfi :yards 1 :feet 1 :inches 1)))
   (cl:= 1 (yards (+ (make-yfi :inches 12) (make-yfi :inches 12) (make-yfi :inches 12)))) ))

(deftest test-+ ()
  (check
   (typep (+) 'yfi)
   (typep (+ 1) 'yfi)
   (= 0 (+))
   (= 1 (+ 0 1))
   (= 1 (+ (make-yfi) (make-yfi :inches 1)))
   (= (+ (make-yfi :inches 20) (make-yfi :inches 30))
      (+ (make-yfi :inches 30) (make-yfi :inches 20)))
   (= (+ (make-yfi :yards 1 :feet 2 :inches 3) (make-yfi :yards 4 :feet 5 :inches 6))
      (+ (make-yfi :yards 4 :feet 5 :inches 6) (make-yfi :yards 1 :feet 2 :inches 3)))
   (= (loop for i from 1 to 10 summing i)
      (apply #'+ (loop for i from 1 to 10 collect i))
      (apply #'+ (mapcar #'(lambda (inches) (make-instance 'yfi :length inches)) (loop for i from 1 to 10 collect i)))) ))

(deftest test-= ()
  (check
   (= 0)
   (= 1 1)
   (not (= 0 1))
   (= (make-yfi :inches 5) (+ 2 3) (+ (make-yfi :inches 2) (make-yfi :inches 3)))) )
