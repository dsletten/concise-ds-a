;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               test-cyclic-counter.lisp
;;;;
;;;;   Started:            Fri Mar  1 02:50:11 2024
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
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :cyclic-counter)

(use-package :test)

(deftest test-make-counter ()
  (check
   (zerop (index (make-counter 8)))
   (let ((n 10))
     (= n (modulus (make-counter n))))
   (handler-case (make-counter 0)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with modulus of 0.~%")))
   (handler-case (make-counter 2.3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with non-integer modulus.~%")))
   (zerop (index (make-instance 'cyclic-counter)))
   (= 1 (modulus (make-instance 'cyclic-counter)))) )

(deftest test-advance ()
  (check
   (let ((c (make-counter 10)))
     (advance c)
     (= 1 (index c)))
   (let* ((n 10)
          (c (make-counter n)))
     (advance c n)
     (zerop (index c)))
   (let* ((n 10)
          (c (make-counter n)))
     (advance c -2)
     (= (- n 2) (index c)))) )

(deftest test-set ()
  (check
   (let ((c (make-counter 10)))
     (advance c)
     (set c 0)
     (zerop (index c)))
   (let ((c (make-counter 10)))
     (advance c 2)
     (set c 0)
     (zerop (index c)))
   (let* ((n 10)
          (c (make-counter n)))
     (set c -4)
     (= (- n 4) (index c)))
   (let* ((n 10)
          (m 6)
          (c (make-counter n)))
     (advance c)
     (set c m)
     (= (mod m n) (index c)))
   (let* ((n 10)
          (m 16)
          (c (make-counter n)))
     (set c m)
     (= (mod m n) (index c)))) )

(deftest test-reset ()
  (check
   (let ((c (make-counter 10)))
     (advance c)
     (reset c)
     (zerop (index c)))
   (let* ((n 10)
          (c (make-counter n)))
     (set c (1- n))
     (reset c)
     (zerop (index c)))) )

(deftest test-rollover ()
  (check
   (loop with n = 10
         with c = (make-counter n)
         repeat n
         do (advance c)
         finally (return (zerop (index c)))) ))

(deftest test-counter ()
  (combine-results
   (test-make-counter)
   (test-advance)
   (test-set)
   (test-reset)
   (test-rollover)))
