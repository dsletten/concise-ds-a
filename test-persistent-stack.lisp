;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-persistent-stack.lisp
;;;;
;;;;   Started:            Tue Mar  2 18:12:38 2021
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

(in-package :containers)

(use-package :test)

(defmethod fill ((stack persistent-stack) &optional (count 1000))
  (loop for i from 1 to count
        for new-stack = (push stack i) then (push new-stack i)   ; ??????? Scope problem without renaming?!??!
        finally (return new-stack)))

(defun test-persistent-stack-constructor (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (assert (emptyp stack) () "New stack should be empty.")
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
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

(defun test-persistent-stack-emptyp (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (assert (emptyp stack) () "New stack should be empty.")
    (assert (not (emptyp (push stack t))) () "Stack with elt should not be empty.")
    (assert (emptyp (pop (push stack t))) () "Empty stack should be empty.")
    t))

(defun test-persistent-stack-size (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
    (loop for i from 1 to count
          for new-stack = (push stack i) then (push new-stack i)   ; ??????? Scope problem without renaming?!??!
          do (assert (= (size new-stack) i) () "Size of stack should be ~D." i)
          finally (return t))))

(defun test-persistent-stack-clear (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) count)))
    (assert (not (emptyp stack)) () "Stack should have ~D elements." count)
    (assert (emptyp (clear stack)) () "Stack should be empty."))
  t)

(defun test-persistent-stack-pop (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) count)))
    (loop for i from (size stack) downto 1
          for (new-stack popped) = (multiple-value-list (pop stack)) then (multiple-value-list (pop new-stack)) 
          unless (= i popped)
          do (error "Wrong value on stack: ~A should be: ~A~%" popped i)
          finally (assert (emptyp new-stack) () "Stack should be empty.")))
  t)

(defun test-persistent-stack-peek (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) count)))
    (loop for i from (size stack) downto 1
          for peek = (peek stack)
          unless (= i peek)
          do (error "Wrong value on stack: ~A should be: ~A~%" peek i)
          do (setf stack (pop stack)))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

(deftest test-persistent-stack ()
  (check
   (test-persistent-stack-constructor #'(lambda () (make-instance 'persistent-stack)))
   (test-persistent-stack-emptyp #'(lambda () (make-instance 'persistent-stack)))
   (test-persistent-stack-size #'(lambda () (make-instance 'persistent-stack)))
   (test-persistent-stack-clear #'(lambda () (make-instance 'persistent-stack)))
   (test-persistent-stack-pop #'(lambda () (make-instance 'persistent-stack)))
   (test-persistent-stack-peek #'(lambda () (make-instance 'persistent-stack)))) )
