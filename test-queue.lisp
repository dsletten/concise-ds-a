;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               test-queue.lisp
;;;;
;;;;   Started:            Thu Mar 11 16:52:43 2021
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

(defmethod fill ((queue queue) &optional (count 1000))
  (loop for i from 1 to count
        do (enqueue queue i)
        finally (return queue)))

(defun test-queue-constructor (queue-constructor)
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

(defun test-queue-emptyp (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (assert (emptyp queue) () "New queue should be empty.")
    (enqueue queue t)
    (assert (not (emptyp queue)) () "Queue with elt should not be empty.")
    (dequeue queue)
    (assert (emptyp queue) () "Empty queue should be empty.")
    t))

(defun test-queue-size (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (assert (zerop (size queue)) () "Size of new queue should be zero.")
    (loop for i from 1 to count
          do (enqueue queue i)
             (assert (= (size queue) i) () "Size of queue should be ~D." i)
          finally (return t))))

(defun test-queue-clear (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) count)))
    (assert (not (emptyp queue)) () "Queue should have ~D elements." count)
    (clear queue)
    (assert (emptyp queue) () "Queue should be empty.")
    (fill queue 1000)
    (assert (not (emptyp queue)) () "Emptying queue should not break it.")
    t))

(defun test-queue-dequeue (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) count)))
    (loop for i from 1 upto (size queue)
          for dequeued = (dequeue queue)
          unless (= i dequeued)
          do (error "Wrong value on queue: ~A should be: ~A~%" dequeued i))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(defun test-queue-front (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) count)))
    (loop for i from 1 upto (size queue)
          for front = (front queue)
          unless (= i front)
          do (error "Wrong value on queue: ~A should be: ~A~%" front i)
          do (dequeue queue))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(defun test-deque-dequeue* (deque-constructor &optional (count 1000))
  (let ((deque (fill (funcall deque-constructor) count)))
    (loop for i from count downto 1
          for dequeued = (dequeue* deque)
          unless (= i dequeued)
          do (error "Wrong value on deque: ~A should be: ~A~%" dequeued i))
    (assert (emptyp deque) () "Deque should be empty."))
  t)

(defun test-deque-rear (deque-constructor &optional (count 1000))
  (let ((deque (fill (funcall deque-constructor) count)))
    (loop for i from count downto 1
          for rear = (rear deque)
          unless (= i rear)
          do (error "Wrong value on deque: ~A should be: ~A~%" rear i)
          do (dequeue* deque))
    (assert (emptyp deque) () "Deque should be empty."))
  t)

(defun test-queue-time (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (time
     (dotimes (i 10 t)
       (dotimes (j 10000)
         (enqueue queue j))
       (loop until (emptyp queue) do (dequeue queue)))) ))

(defun test-queue-wave (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (fill queue 5000)
    (assert (= (size queue) 5000))
    (dotimes (i 3000)
      (dequeue queue))
    (assert (= (size queue) 2000))
    (fill queue 5000)
    (assert (= (size queue) 7000))
    (dotimes (i 3000)
      (dequeue queue))
    (assert (= (size queue) 4000))
    (fill queue 5000)
    (assert (= (size queue) 9000))
    (dotimes (i 3000)
      (dequeue queue))
    (assert (= (size queue) 6000))
    (fill queue 4000)
    (assert (= (size queue) 10000))
    (dotimes (i 10000)
      (dequeue queue))
    (assert (emptyp queue)))
  t)

(deftest test-array-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'array-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'array-queue)))
   (test-queue-size #'(lambda () (make-instance 'array-queue)))
   (test-queue-clear #'(lambda () (make-instance 'array-queue)))
   (test-queue-dequeue #'(lambda () (make-instance 'array-queue)))
   (test-queue-front #'(lambda () (make-instance 'array-queue)))
   (test-queue-time #'(lambda () (make-instance 'array-queue)))
   (test-queue-wave #'(lambda () (make-instance 'array-queue)))) )

(deftest test-linked-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'linked-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'linked-queue)))
   (test-queue-size #'(lambda () (make-instance 'linked-queue)))
   (test-queue-clear #'(lambda () (make-instance 'linked-queue)))
   (test-queue-dequeue #'(lambda () (make-instance 'linked-queue)))
   (test-queue-front #'(lambda () (make-instance 'linked-queue)))
   (test-queue-time #'(lambda () (make-instance 'linked-queue)))
   (test-queue-wave #'(lambda () (make-instance 'linked-queue)))) )

(deftest test-linked-list-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-size #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-clear #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-dequeue #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-front #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-time #'(lambda () (make-instance 'linked-list-queue)))
   (test-queue-wave #'(lambda () (make-instance 'linked-list-queue)))) )

(deftest test-circular-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'circular-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'circular-queue)))
   (test-queue-size #'(lambda () (make-instance 'circular-queue)))
   (test-queue-clear #'(lambda () (make-instance 'circular-queue)))
   (test-queue-dequeue #'(lambda () (make-instance 'circular-queue)))
   (test-queue-front #'(lambda () (make-instance 'circular-queue)))
   (test-queue-time #'(lambda () (make-instance 'circular-queue)))
   (test-queue-wave #'(lambda () (make-instance 'circular-queue)))) )

(deftest test-recycling-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'recycling-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'recycling-queue)))
   (test-queue-size #'(lambda () (make-instance 'recycling-queue)))
   (test-queue-clear #'(lambda () (make-instance 'recycling-queue))) 
   (test-queue-dequeue #'(lambda () (make-instance 'recycling-queue)))
   (test-queue-front #'(lambda () (make-instance 'recycling-queue)))
   (test-queue-time #'(lambda () (make-instance 'recycling-queue))) 
   (test-queue-wave #'(lambda () (make-instance 'recycling-queue)))) )

(deftest test-ring-buffer ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'ring-buffer)))
   (test-queue-emptyp #'(lambda () (make-instance 'ring-buffer)))
   (test-queue-size #'(lambda () (make-instance 'ring-buffer)))
   (test-queue-clear #'(lambda () (make-instance 'ring-buffer))) 
   (test-queue-dequeue #'(lambda () (make-instance 'ring-buffer)))
   (test-queue-front #'(lambda () (make-instance 'ring-buffer)))
   (test-queue-time #'(lambda () (make-instance 'ring-buffer))) 
   (test-queue-wave #'(lambda () (make-instance 'ring-buffer)))) )

(deftest test-hash-table-queue ()
  (check
   (test-queue-constructor #'(lambda () (make-instance 'hash-table-queue)))
   (test-queue-emptyp #'(lambda () (make-instance 'hash-table-queue)))
   (test-queue-size #'(lambda () (make-instance 'hash-table-queue)))
   (test-queue-clear #'(lambda () (make-instance 'hash-table-queue))) 
   (test-queue-dequeue #'(lambda () (make-instance 'hash-table-queue)))
   (test-queue-front #'(lambda () (make-instance 'hash-table-queue)))
   (test-queue-time #'(lambda () (make-instance 'hash-table-queue))) 
   (test-queue-wave #'(lambda () (make-instance 'hash-table-queue)))) )

(deftest test-dll-deque ()
  (check
   (test-queue-constructor #'make-dll-deque)
   (test-queue-emptyp #'make-dll-deque)
   (test-queue-size #'make-dll-deque)
   (test-queue-clear #'make-dll-deque)
   (test-queue-dequeue #'make-dll-deque)
   (test-queue-front #'make-dll-deque)
   (test-deque-dequeue* #'make-dll-deque)
   (test-deque-rear #'make-dll-deque)
   (test-queue-time #'make-dll-deque)
   (test-queue-wave #'make-dll-deque)))

(deftest test-hash-table-deque ()
  (check
   (test-queue-constructor #'make-hash-table-deque)
   (test-queue-emptyp #'make-hash-table-deque)
   (test-queue-size #'make-hash-table-deque)
   (test-queue-clear #'make-hash-table-deque)
   (test-queue-dequeue #'make-hash-table-deque)
   (test-queue-front #'make-hash-table-deque)
   (test-deque-dequeue* #'make-hash-table-deque)
   (test-deque-rear #'make-hash-table-deque)
   (test-queue-time #'make-hash-table-deque)
   (test-queue-wave #'make-hash-table-deque)))

(deftest test-queue-all ()
  (check
   (test-array-queue)
   (test-linked-queue)
   (test-linked-list-queue)
   (test-circular-queue)
   (test-recycling-queue)
   (test-ring-buffer)
   (test-hash-table-queue)
   (test-dll-deque)
   (test-hash-table-deque)))
