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
       (error "Can't create counter with modulus of 0.~%")))) )

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
     (= (- n 2) (index c)))))

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
     (= (mod m n) (index c)))))

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
     (zerop (index c)))))

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

(deftest test-make-persistent-counter ()
  (check
   (zerop (index (make-persistent-counter 8)))
   (let ((n 10))
     (= n (modulus (make-persistent-counter n))))
   (handler-case (make-persistent-counter 0)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with modulus of 0.~%")))) )

(deftest test-persistent-counter-advance ()
  (check
   (= 1 (index (advance (make-persistent-counter 10))))
   (let ((n 10))
     (zerop (index (advance (make-persistent-counter n) n))))
   (let ((n 10))
     (= (- n 2) (index (advance (make-persistent-counter n) -2)))) ))

(deftest test-persistent-counter-set ()
  (check
   (zerop (index (set (advance (make-persistent-counter 10)) 0)))
   (zerop (index (set (advance (make-persistent-counter 10) 2) 0)))
   (let ((n 10))
     (= (- n 4) (index (set (make-persistent-counter n) -4))))
   (let ((n 10)
         (m 6))
     (= (mod m n) (index (set (advance (make-persistent-counter n)) m))))
   (let ((n 10)
         (m 16))
     (= (mod m n) (index (set (make-persistent-counter n) m)))) ))

(deftest test-persistent-counter-reset ()
  (check
   (zerop (index (reset (advance (make-persistent-counter 10)))))
   (let ((n 10))
     (zerop (index (reset (set (make-persistent-counter n) (1- n)))) ))))

(deftest test-persistent-counter-rollover ()
  (check
   (loop with n = 10
         repeat n
         for c = (advance (make-persistent-counter n)) then (advance c)
         finally (return (zerop (index c)))) ))

(deftest test-persistent-counter ()
  (combine-results
   (test-make-persistent-counter)
   (test-persistent-counter-advance)
   (test-persistent-counter-set)
   (test-persistent-counter-reset)
   (test-persistent-counter-rollover)))
