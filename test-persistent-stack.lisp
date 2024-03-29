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
    (setf stack (push stack t))
    (assert (not (emptyp stack)) () "Stack with elt should not be empty.")
    (setf stack (pop stack))
    (assert (emptyp stack) () "Empty stack should be empty.")
    t))

(defun test-persistent-stack-size (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
    (loop for i from 1 to count
          do (setf stack (push stack i))
             (assert (= (size stack) i) () "Size of stack should be ~D." i))
    (loop for i from (1- count) downto 0
          do (setf stack (pop stack))
             (assert (= (size stack) i) () "Size of stack should be ~D." i))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

(defun test-persistent-stack-clear (stack-constructor &optional (count 1000))
  (let ((original-stack (fill (funcall stack-constructor) :count count)))
    (assert (not (emptyp original-stack)) () "Stack should have ~D elements." count)
    (let ((stack (clear original-stack)))
      (assert (emptyp stack) () "Stack should be empty.")
      (assert (not (emptyp original-stack)) () "Original stack is unaffected.")
      (assert (not (eq stack original-stack)) () "Cleared stack is new stack.")
      (assert (zerop (size stack)) () "Size of empty stack should be 0.")
      (assert (eq stack (clear stack)) () "Clearing empty stack has no effect.")))
  t)

(defun test-persistent-stack-elements (stack-constructor &optional (count 1000))
  (let* ((stack (fill (funcall stack-constructor) :count count))
         (expected (loop for i from count downto 1 collect i))
         (elements (elements stack)))
    (assert (equal expected elements) () "LIFO elements should be ~A not ~A" (subseq expected 0 10) (subseq elements 0 10)))
  t)
    
(defun test-persistent-stack-push (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (loop for i from 1 to count
          do (setf stack (push stack i))
             (assert (= i (peek stack)) () "Wrong value pushed: ~A should be: ~A~%" (peek stack) i)))
  t)

(defun test-persistent-stack-push-wrong-type (stack-constructor)
  (let ((stack (funcall stack-constructor :type 'integer)))
    (handler-case (push stack 1d0)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't PUSH value of wrong type onto stack.~%"))))
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
                 test-persistent-stack-elements
                 test-persistent-stack-push
                 test-persistent-stack-push-wrong-type
                 test-persistent-stack-peek-pop
                 test-persistent-stack-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-persistent-linked-stack ()
  (check
   (persistent-stack-test-suite #'(lambda (&key (type t)) (make-instance 'persistent-linked-stack :type type)))) )

(deftest test-persistent-list-stack ()
  (check
   (persistent-stack-test-suite #'(lambda (&key (type t)) (make-instance 'persistent-list-stack :type type)))) )

(deftest test-persistent-stack-all ()
  (check
   (test-persistent-linked-stack)
   (test-persistent-list-stack)))
   
