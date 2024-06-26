;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               test-yfi.lisp
;;;;
;;;;   Started:            Sat May  4 03:47:53 2024
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

(in-package :yfi)

(use-package :test)

(deftest test-make-yfi ()
  (check
   (handler-case (make-yfi 1 -2 3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Length components must be non-negative integers.")))
   (handler-case (make-yfi 1.0 2.0 3.0)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Length components must be non-negative integers.")))) )

(deftest test-length ()
  (check
   (cl:= 0 (length (make-yfi)))
   (cl:= 1 (length (make-yfi 1)))
   (cl:= 12 (length (make-yfi 1 0)))
   (cl:= 36 (length (make-yfi 1 0 0)))
   (cl:= 49 (length (make-yfi 1 1 1)))
   (cl:= 100 (length (+ (make-yfi 49) (make-yfi 51)))) ))

(deftest test-inches ()
  (check
   (cl:= 0 (inches (make-yfi)))
   (cl:= 1 (inches (make-yfi 1)))
   (cl:= 0 (inches (make-yfi 1 0)))
   (cl:= 0 (inches (make-yfi 1 0 0)))
   (cl:= 1 (inches (make-yfi 1 1 1)))
   (cl:= 0 (inches (+ (make-yfi 8) (make-yfi 4)))) ))

(deftest test-feet ()
  (check
   (cl:= 0 (feet (make-yfi)))
   (cl:= 0 (feet (make-yfi 1)))
   (cl:= 1 (feet (make-yfi 1 0)))
   (cl:= 0 (feet (make-yfi 1 0 0)))
   (cl:= 1 (feet (make-yfi 1 1 1)))
   (cl:= 0 (feet (+ (make-yfi 16) (make-yfi 20)))) ))

(deftest test-yards ()
  (check
   (cl:= 0 (yards (make-yfi)))
   (cl:= 0 (yards (make-yfi 1)))
   (cl:= 0 (yards (make-yfi 1 0)))
   (cl:= 1 (yards (make-yfi 1 0 0)))
   (cl:= 1 (yards (make-yfi 1 1 1)))
   (cl:= 1 (yards (+ (make-yfi 12) (make-yfi 12) (make-yfi 12)))) ))

(deftest test-+ ()
  (check
   (typep (+) 'yfi)
   (typep (+ (make-yfi 1)) 'yfi)
   (= (make-yfi) (+))
   (let ((a (make-yfi 1)))
     (and (= a (+ (make-yfi) a))
          (= a (+ a (make-yfi)))) )
   (let ((a (make-yfi 20))
         (b (make-yfi 30)))
     (= (+ a b) (+ b a)))
   (let ((a (make-yfi 1 2 3))
         (b (make-yfi 4 5 6)))
     (= (+ a b) (+ b a)))
   (let ((a (make-yfi 20))
         (b (make-yfi 30))
         (c (make-yfi 40)))
     (= (+ (+ a b) c) (+ a (+ b c)) (+ a b c)))
   (cl:= (loop for i from 1 to 10 summing i)
         (length (apply #'+ (mapcar #'make-yfi (loop for i from 1 to 10 collect i)))) )))

(deftest test-= ()
  (check
   (= (make-yfi))
   (= (make-yfi 1) (make-yfi 1))
   (not (= (+) (make-yfi 1)))
   (= (make-yfi 5)
      (+ (make-yfi 2) (make-yfi 3)))
   (let ((a (+ (make-yfi 20) (make-yfi 19)))
         (b (make-yfi 39))
         (c (make-yfi 1 0 3)))
     (and (= a b c)
          (= a c b)
          (= b a c)
          (= b c a)
          (= c a b)
          (= c b a)))
   (let ((random-state (make-random-state t)))
     (dotimes (i 100 t)
       (let* ((length (random 200 random-state))
              (yfi1 (make-instance 'yfi :length length))
              (yfi2 (make-yfi (yards yfi1) (feet yfi1) (inches yfi1))))
         (unless (= yfi1 yfi2)
           (return nil)))) )))
   
(deftest test-yfi ()
  (combine-results
   (test-make-yfi)
   (test-length)
   (test-inches)
   (test-feet)
   (test-yards)
   (test-+)
   (test-=)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :yfi-keys)

(use-package :test)

(deftest test-make-yfi ()
  (check
   (handler-case (make-yfi :yards 1 :feet -2 :inches 3)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Length components must be non-negative integers.")))
   (handler-case (make-yfi :yards 1.0 :feet 2.0 :inches 3.0)
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
   (cl:= 100 (length (+ (make-yfi :inches 49) (make-yfi :inches 51)))) ))

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
   (typep (+ (make-yfi :inches 1)) 'yfi)
   (= (make-yfi) (+))
   (let ((a (make-yfi :inches 1)))
     (and (= a (+ (make-yfi) a))
          (= a (+ a (make-yfi)))) )
   (let ((a (make-yfi :inches 20))
         (b (make-yfi :inches 30)))
     (= (+ a b) (+ b a)))
   (let ((a (make-yfi :yards 1 :feet 2 :inches 3))
         (b (make-yfi :yards 4 :feet 5 :inches 6)))
     (= (+ a b) (+ b a)))
   (let ((a (make-yfi :inches 20))
         (b (make-yfi :inches 30))
         (c (make-yfi :inches 40)))
     (= (+ (+ a b) c) (+ a (+ b c)) (+ a b c)))
   (cl:= (loop for i from 1 to 10 summing i)
         (length (apply #'+ (mapcar #'(lambda (inches) (make-instance 'yfi :length inches)) (loop for i from 1 to 10 collect i)))) )))

(deftest test-= ()
  (check
   (= (make-yfi))
   (= (make-yfi :inches 1) (make-yfi :inches 1))
   (not (= (+) (make-yfi :inches 1)))
   (= (make-yfi :inches 5)
      (+ (make-yfi :inches 2) (make-yfi :inches 3)))
   (let ((a (+ (make-yfi :inches 20) (make-yfi :inches 19)))
         (b (make-yfi :inches 39))
         (c (make-yfi :yards 1 :inches 3)))
     (and (= a b c)
          (= a c b)
          (= b a c)
          (= b c a)
          (= c a b)
          (= c b a)))
   (let ((random-state (make-random-state t)))
     (dotimes (i 100 t)
       (let* ((length (random 200 random-state))
              (yfi1 (make-instance 'yfi :length length))
              (yfi2 (make-yfi :inches (inches yfi1) :feet (feet yfi1) :yards (yards yfi1))))
         (unless (= yfi1 yfi2)
           (return nil)))) )))
   
(deftest test-yfi ()
  (combine-results
   (test-make-yfi)
   (test-length)
   (test-inches)
   (test-feet)
   (test-yards)
   (test-+)
   (test-=)))
