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

(defun test-constructor (stack-constructor)
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

(defun test-emptyp (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (assert (emptyp stack) () "New stack should be empty.")
    (push stack t)
    (assert (not (emptyp stack)) () "Stack with elt should not be empty.")
    (pop stack)
    (assert (emptyp stack) () "Empty stack should be empty.")
    t))

(defun test-size (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (assert (zerop (size stack)) () "Size of new stack should be zero.")
    (loop for i from 1 to count
          do (push stack i)
             (assert-stack-size stack i)
          finally (return t))))

(defun assert-stack-size (stack n)
  (assert (= (size stack) n) () "Size of stack should be ~D." n))

(defun test-clear (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (fill-stack stack count)
    (assert (not (emptyp stack)) () "Stack should have ~D elements." count)
    (clear stack)
    (assert (emptyp stack) () "Stack should be empty.")
    t))

(defun fill-stack (stack count)
  (loop for i from 1 to count
        do (push stack i)))

;; (defun test-pop (stack-constructor &optional (count 1000))
;;   (labels ((test-recursive (stack n)
;;              (cond ((emptyp stack) t)
;;                    ((= n (pop stack)) (test-recursive stack (1- n)))
;;                    (t (error "Wrong value on stack: ~A should be: ~A~%" (top stack) n))))) ; Value already popped!!!
;;     (let ((stack (funcall stack-constructor)))
;;       (fill-stack stack count)
;;       (test-recursive stack (size stack)))) )

(defun test-pop (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (fill-stack stack count)
    (loop for i from (size stack) downto 1
          for popped = (pop stack)
          unless (= i popped)
          do (error "Wrong value on stack: ~A should be: ~A~%" popped i))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

;; (defun test-top (stack-constructor &optional (count 1000))
;;   (labels ((test-recursive (stack n)
;;              (cond ((emptyp stack) t)
;;                    ((= n (top stack)) (pop stack) (test-recursive stack (1- n)))
;;                    (t (error "Wrong value on stack: ~A should be: ~A~%" (top stack) n)))))
;;     (let ((stack (funcall stack-constructor)))
;;       (fill-stack stack count)
;;       (test-recursive stack (size stack)))) )

(defun test-peek (stack-constructor &optional (count 1000))
  (let ((stack (funcall stack-constructor)))
    (fill-stack stack count)
    (loop for i from (size stack) downto 1
          for top = (peek stack)
          unless (= i top)
          do (error "Wrong value on stack: ~A should be: ~A~%" top i)
          do (pop stack))
    (assert (emptyp stack) () "Stack should be empty."))
  t)

(defun test-time (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (time
     (dotimes (i 10 t)
       (dotimes (j 10000)
         (push stack j))
       (loop until (emptyp stack) do (pop stack)))) ))

(defun test-wave (stack-constructor)
  (let ((stack (funcall stack-constructor)))
    (fill-stack stack 5000)
    (assert (= (size stack) 5000))
    (dotimes (i 3000)
      (pop stack))
    (assert (= (size stack) 2000))
    (fill-stack stack 5000)
    (assert (= (size stack) 7000))
    (dotimes (i 3000)
      (pop stack))
    (assert (= (size stack) 4000))
    (fill-stack stack 5000)
    (assert (= (size stack) 9000))
    (dotimes (i 3000)
      (pop stack))
    (assert (= (size stack) 6000))
    (fill-stack stack 4000)
    (assert (= (size stack) 10000))
    (dotimes (i 10000)
      (pop stack))
    (assert (emptyp stack)))
  t)

(deftest test-linked-stack ()
  (check
   (test-constructor #'(lambda () (make-instance 'linked-stack)))
   (test-emptyp #'(lambda () (make-instance 'linked-stack)))
   (test-size #'(lambda () (make-instance 'linked-stack)))
   (test-clear #'(lambda () (make-instance 'linked-stack)))
   (test-pop #'(lambda () (make-instance 'linked-stack)))
   (test-peek #'(lambda () (make-instance 'linked-stack)))
   (test-time #'(lambda () (make-instance 'linked-stack)))
   (test-wave #'(lambda () (make-instance 'linked-stack)))) )

(deftest test-array-stack ()
  (check
   (test-constructor #'(lambda () (make-instance 'array-stack)))
   (test-emptyp #'(lambda () (make-instance 'array-stack)))
   (test-size #'(lambda () (make-instance 'array-stack)))
   (test-clear #'(lambda () (make-instance 'array-stack)))
   (test-pop #'(lambda () (make-instance 'array-stack)))
   (test-peek #'(lambda () (make-instance 'array-stack)))
   (test-time #'(lambda () (make-instance 'array-stack)))
   (test-wave #'(lambda () (make-instance 'array-stack)))) )

(deftest test-hash-table-stack ()
  (check
   (test-constructor #'(lambda () (make-instance 'hash-table-stack)))
   (test-emptyp #'(lambda () (make-instance 'hash-table-stack)))
   (test-size #'(lambda () (make-instance 'hash-table-stack)))
   (test-clear #'(lambda () (make-instance 'hash-table-stack))) 
   (test-pop #'(lambda () (make-instance 'hash-table-stack)))
   (test-peek #'(lambda () (make-instance 'hash-table-stack)))
   (test-time #'(lambda () (make-instance 'hash-table-stack))) 
   (test-wave #'(lambda () (make-instance 'hash-table-stack)))) )

