;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-persistent-cyclic-counter.lisp
;;;;
;;;;   Started:            Fri Apr 12 23:08:51 2024
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
       (error "Can't create counter with modulus of 0.~%")))
   (handler-case (make-persistent-counter 2.3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with non-integer modulus.~%")))
   (zerop (index (make-instance 'persistent-cyclic-counter)))
   (= 1 (modulus (make-instance 'persistent-cyclic-counter)))
   (handler-case (make-instance 'persistent-cyclic-counter :index 5)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't create counter with default modulus and specified index.~%")))) )

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
   (zerop (index (reset (advance (make-persistent-counter 10)))) )
   (let ((n 10))
     (zerop (index (reset (set (make-persistent-counter n) (1- n)))) ))))

(deftest test-persistent-counter-rollover ()
  (check
   (loop with n = 10
         repeat n
         for c = (advance (make-persistent-counter n)) then (advance c)
         finally (return (zerop (index c)))) ))

;;;
;;;    Same semantics of Ruby/JavaScript
;;;    
(deftest test-persistent-counter-rollover ()
  (check
   (let* ((n 10)
          (c (make-persistent-counter n)))
     (loop repeat n
           do (setf c (advance c)))
     (zerop (index c)))) )

;;;
;;;    More like semantics of Clojure version (loop/recur).
;;;    
(deftest test-persistent-counter-rollover ()
  (check
   (do* ((n 10)
         (i 0 (1+ i))
         (c (make-persistent-counter n) (advance c)))
        ((= i n) (zerop (index c)))) ))

(deftest test-persistent-counter ()
  (combine-results
   (test-make-persistent-counter)
   (test-persistent-counter-advance)
   (test-persistent-counter-set)
   (test-persistent-counter-reset)
   (test-persistent-counter-rollover)))
