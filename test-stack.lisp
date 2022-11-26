;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               test-stack.lisp
;;;;
;;;;   Started:            Thu Apr 11 14:02:44 2013
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
;;;;   Copied from Foundations of OOP code
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

;;;
;;;    Not appropriate for PERSISTENT-STACK!
;;;    
(defmethod fill ((stack stack) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        do (push stack (funcall generator i))
        finally (return stack)))

(defun test-stack-constructor (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (assert (emptyp stack) () "New stack should be empty.")
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
;    (assert-stack-size stack 0)
    (handler-case (peek stack)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call PEEK on empty stack.~%")))
    (handler-case (pop stack)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call POP on empty stack.~%")))
    t))

(defun test-stack-emptyp (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (assert (emptyp stack) () "New stack should be empty.")
    (push stack t)
    (assert (not (emptyp stack)) () "Stack with elt should not be empty.")
    (pop stack)
    (assert (emptyp stack) () "Empty stack should be empty.")
    t))

(defun test-stack-size (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
;    (assert-stack-size stack 0)
    (loop for i from 1 to count
          do (push stack i)
             (assert-stack-size stack i)
          finally (return t))))

(defun assert-stack-size (stack n)
  (assert (= (size stack) n) () "Size of stack should be ~D." n))

(defun test-stack-clear (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) :count count)))
    (assert (not (emptyp stack)) () "Stack should have ~D element~:P." count)
    (clear stack)
    (assert (emptyp stack) () "Stack should be empty.")
    (assert-stack-size stack 0)
    t))

(defun test-stack-peek-pop (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) :count count)))
    (loop repeat (size stack)
          for top = (peek stack)
          for popped = (pop stack)
          do (assert (= top popped) () "Wrong value popped: ~A should be: ~A~%" popped top))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

(defun test-stack-time (stack-constructor &optional (count 100000))
  (let ((stack (funcall stack-constructor)))
    (time
     (dotimes (i 10 t)
       (fill stack :count count)
       (loop until (emptyp stack) do (pop stack)))) ))

(defun test-stack-wave (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (fill stack :count 5000)
    (assert-stack-size stack 5000)
    (dotimes (i 3000)
      (pop stack))
    (assert-stack-size stack 2000)
    (fill stack :count 5000)
    (assert-stack-size stack 7000)
    (dotimes (i 3000)
      (pop stack))
    (assert-stack-size stack 4000)
    (fill stack :count 5000)
    (assert-stack-size stack 9000)
    (dotimes (i 3000)
      (pop stack))
    (assert-stack-size stack 6000)
    (fill stack :count 4000)
    (assert-stack-size stack 10000)
    (dotimes (i 10000)
      (pop stack))
    (assert (emptyp stack)))
  t)

(deftest stack-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-stack-constructor
                 test-stack-emptyp
                 test-stack-size
                 test-stack-clear
                 test-stack-peek-pop
                 test-stack-time
                 test-stack-wave)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-linked-stack ()
  (check
   (stack-test-suite #'(lambda () (make-instance 'linked-stack)))) )

(deftest test-linked-list-stack ()
  (check
   (stack-test-suite #'(lambda () (make-instance 'linked-list-stack)))) )

(deftest test-array-stack ()
  (check
   (stack-test-suite #'(lambda () (make-instance 'array-stack)))) )

(deftest test-hash-table-stack ()
  (check
   (stack-test-suite #'(lambda () (make-instance 'hash-table-stack)))) )

(deftest test-stack-all ()
  (check
   (test-linked-stack)
   (test-linked-list-stack)
   (test-array-stack)
   (test-hash-table-stack)))
