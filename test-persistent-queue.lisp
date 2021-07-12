;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-persistent-queue.lisp
;;;;
;;;;   Started:            Fri Mar 26 00:01:56 2021
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

(defun test-persistent-constructor (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (assert (emptyp queue) () "New queue should be empty.")
    (assert (zerop (size queue)) () "Size of new queue should be zero.")
    (handler-case (front queue)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call FRONT on empty queue.~%")))
    (handler-case (dequeue queue)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DEQUEUE on empty queue.~%")))
    t))

(defun test-persistent-emptyp (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (assert (emptyp queue) () "New queue should be empty.")
    (assert (not (emptyp (enqueue queue t))) () "Queue with elt should not be empty.")
    (assert (emptyp (dequeue (enqueue queue t))) () "Empty queue should be empty.")
    t))

(defun test-persistent-size (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (assert (zerop (size queue)) () "Size of new queue should be zero.")
    (loop for i from 1 to count
          for new-queue = (enqueue queue i) then (enqueue new-queue i)   ; ??????? Scope problem without renaming?!??!
          do (assert (= (size new-queue) i) () "Size of queue should be ~D." i)
          finally (return t))))

(defun test-persistent-clear (queue-constructor &optional (count 1000))
  (let ((queue (fill-persistent-queue (funcall queue-constructor) count)))
    (assert (not (emptyp queue)) () "Queue should have ~D elements." count)
    (assert (emptyp (clear queue)) () "Queue should be empty."))
  t)

(defun fill-persistent-queue (queue count)
  (loop for i from 1 to count
        for new-queue = (enqueue queue i) then (enqueue new-queue i)   ; ??????? Scope problem without renaming?!??!
        finally (return new-queue)))

(defun test-persistent-dequeue (queue-constructor &optional (count 1000))
  (let ((queue (fill-persistent-queue (funcall queue-constructor) count)))
    (loop for i from 1 upto (size queue)
          for (new-queue dequeued) = (multiple-value-list (dequeue queue)) then (multiple-value-list (dequeue new-queue)) 
          unless (= i dequeued)
          do (error "Wrong value on queue: ~A should be: ~A~%" dequeued i)
          finally (assert (emptyp new-queue) () "Queue should be empty.")))
  t)

(defun test-persistent-front (queue-constructor &optional (count 1000))
  (let ((queue (fill-persistent-queue (funcall queue-constructor) count)))
    (loop for i from 1 upto (size queue)
          for front = (front queue)
          unless (= i front)
          do (error "Wrong value on queue: ~A should be: ~A~%" front i)
          do (setf queue (dequeue queue)))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(deftest test-persistent-queue ()
  (check
   (test-persistent-constructor #'(lambda () (make-instance 'persistent-queue)))
   (test-persistent-emptyp #'(lambda () (make-instance 'persistent-queue)))
   (test-persistent-size #'(lambda () (make-instance 'persistent-queue)))
   (test-persistent-clear #'(lambda () (make-instance 'persistent-queue)))
   (test-persistent-dequeue #'(lambda () (make-instance 'persistent-queue)))
   (test-persistent-front #'(lambda () (make-instance 'persistent-queue)))) )
