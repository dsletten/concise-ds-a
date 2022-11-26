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

(defmethod fill ((stack persistent-stack) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-stack = (push stack (funcall generator i)) then (push new-stack (funcall generator i))   ; ??????? Scope problem without renaming?!??!
        finally (return new-stack)))

;;;
;;;    Refactor? PERSISTENT-STACK/PERSISTENT-LIST-STACK with common superclass??
;;;    
(defmethod fill ((stack persistent-list-stack) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-stack = (push stack (funcall generator i)) then (push new-stack (funcall generator i))   ; ??????? Scope problem without renaming?!??!
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
  (let ((stack (fill (funcall stack-constructor) :count count)))
    (assert (not (emptyp stack)) () "Stack should have ~D elements." count)
    (assert (emptyp (clear stack)) () "Stack should be empty.")
    (assert (zerop (size (clear stack))) () "Size of stack should be 0."))
  t)

(defun test-persistent-stack-peek-pop (stack-constructor &optional (count 1000))
  (let ((stack (fill (funcall stack-constructor) :count count)))
    (loop repeat (size stack)
          for top = (peek stack)
          do (multiple-value-bind (s popped) (pop stack)
               (setf stack s)
               (assert (= top popped) () "Wrong value popped: ~A should be: ~A~%" popped top)))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

(defun test-persistent-stack-time (stack-constructor &optional (count 100000))
  (time
   (dotimes (i 10 t)
     (loop for stack = (fill (funcall stack-constructor) :count count) then (pop stack)
           until (emptyp stack)))) )

(deftest persistent-stack-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-persistent-stack-constructor
                 test-persistent-stack-emptyp
                 test-persistent-stack-size
                 test-persistent-stack-clear
                 test-persistent-stack-peek-pop
                 test-persistent-stack-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-persistent-stack ()
  (check
   (persistent-stack-test-suite #'(lambda () (make-instance 'persistent-stack)))) )

(deftest test-persistent-list-stack ()
  (check
   (persistent-stack-test-suite #'(lambda () (make-instance 'persistent-list-stack)))) )

(deftest test-persistent-stack-all ()
  (check
   (test-persistent-stack)
   (test-persistent-list-stack)))
   
