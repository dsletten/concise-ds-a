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

(defun test-deque-constructor (deque-constructor)
  (let ((deque (funcall deque-constructor)))
    (assert (emptyp deque) () "New deque should be empty.")
    (assert (zerop (size deque)) () "Size of new deque should be zero.")
    (handler-case (rear deque)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call REAR on empty deque.~%")))
    (handler-case (dequeue* deque)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DEQUEUE* on empty deque.~%")))
    t))

(defun test-queue-emptyp (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (assert (emptyp queue) () "New queue should be empty.")
    (enqueue queue t)
    (assert (not (emptyp queue)) () "Queue with elt should not be empty.")
    (dequeue queue)
    (assert (emptyp queue) () "Empty queue should be empty.")
    t))

(defun test-deque-emptyp (deque-constructor)
  (let ((deque (funcall deque-constructor)))
    (assert (emptyp deque) () "New deque should be empty.")
    (enqueue* deque t)
    (assert (not (emptyp deque)) () "Deque with elt should not be empty.")
    (dequeue* deque)
    (assert (emptyp deque) () "Empty deque should be empty.")
    t))

(defun test-queue-size (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (assert (zerop (size queue)) () "Size of new queue should be zero.")
    (loop for i from 1 to count
          do (enqueue queue i)
             (assert-queue-size queue i)
          finally (return t))))

(defun assert-queue-size (queue n)
  (assert (= (size queue) n) () "Size of queue should be ~D." n))

(defun test-deque-size (deque-constructor &optional (count 1000))
  (let ((deque (funcall deque-constructor)))
    (assert (zerop (size deque)) () "Size of new deque should be zero.")
    (loop for i from 1 to count
          do (enqueue* deque i)
             (assert-queue-size deque i)
          finally (return t))))

(defun test-queue-clear (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) :count count)))
    (assert (not (emptyp queue)) () "Queue should have ~D elements." count)
    (clear queue)
    (assert (emptyp queue) () "Queue should be empty.")
    (assert-queue-size queue 0)
    (fill queue :count count)
    (assert (not (emptyp queue)) () "Emptying queue should not break it.")
    t))

(defun test-queue-front-dequeue (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) :count count)))
    (loop repeat (size queue)
          for front = (front queue)
          for dequeued = (dequeue queue)
          do (assert (= front dequeued) () "Wrong value dequeued: ~A should be: ~A~%" dequeued front))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(defun test-deque-rear-dequeue* (deque-constructor &optional (count 1000))
  (let ((deque (fill (funcall deque-constructor) :count count)))
    (loop repeat (size deque)
          for rear = (rear deque)
          for dequeued = (dequeue* deque)
          do (assert (= rear dequeued) () "Wrong value dequeued from rear: ~A should be: ~A~%" dequeued rear))
    (assert (emptyp deque) () "Deque should be empty."))
  t)

(defun test-queue-time (queue-constructor &optional (count 100000))
  (let ((queue (funcall queue-constructor)))
    (time
     (dotimes (i 10 t)
       (fill queue :count count)
       (loop until (emptyp queue) do (dequeue queue)))) ))

(defun test-deque-time (deque-constructor &optional (count 100000))
  (let ((deque (funcall deque-constructor)))
    (time
     (dotimes (i 10 t)
       (dotimes (j count)
         (enqueue* deque j))
       (loop until (emptyp deque) do (dequeue* deque)))) ))

(defun test-queue-wave (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (fill queue :count 5000)
    (assert-queue-size queue 5000)
    (dotimes (i 3000)
      (dequeue queue))
    (assert-queue-size queue 2000)
    (fill queue :count 5000)
    (assert-queue-size queue 7000)
    (dotimes (i 3000)
      (dequeue queue))
    (assert-queue-size queue 4000)
    (fill queue :count 5000)
    (assert-queue-size queue 9000)
    (dotimes (i 3000)
      (dequeue queue))
    (assert-queue-size queue 6000)
    (fill queue :count 4000)
    (assert-queue-size queue 10000)
    (dotimes (i 10000)
      (dequeue queue))
    (assert (emptyp queue)))
  t)

(deftest queue-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-queue-constructor
                 test-queue-emptyp
                 test-queue-size
                 test-queue-clear
                 test-queue-front-dequeue
                 test-queue-time
                 test-queue-wave)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-array-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'array-queue)))) )

(deftest test-linked-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'linked-queue)))) )

(deftest test-linked-list-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'linked-list-queue)))) )

(deftest test-circular-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'circular-queue)))) )

(deftest test-recycling-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'recycling-queue)))) )

(deftest test-ring-buffer ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'ring-buffer)))) )

(deftest test-hash-table-queue ()
  (check
   (queue-test-suite #'(lambda () (make-instance 'hash-table-queue)))) )

(deftest deque-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-queue-constructor
                 test-deque-constructor
                 test-queue-emptyp
                 test-deque-emptyp
                 test-queue-size
                 test-deque-size
                 test-queue-clear
                 test-queue-front-dequeue
                 test-deque-rear-dequeue*
                 test-queue-time
                 test-deque-time
                 test-queue-wave)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-array-deque ()
  (check
   (deque-test-suite #'make-array-deque)))

(deftest test-dll-deque ()
  (check
   (deque-test-suite #'make-dll-deque)))

(deftest test-hash-table-deque ()
  (check
   (deque-test-suite #'make-hash-table-deque)))

(deftest test-queue-all ()
  (check
   (test-array-queue)
   (test-linked-queue)
   (test-linked-list-queue)
   (test-circular-queue)
   (test-recycling-queue)
   (test-ring-buffer)
   (test-hash-table-queue)
   (test-array-deque)
   (test-dll-deque)
   (test-hash-table-deque)))
