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
             (assert-queue-size queue i))
    (loop for i from (1- count) downto 0
          do (dequeue queue)
             (assert-queue-size queue i))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(defun assert-queue-size (queue n)
  (assert (= (size queue) n) () "Size of queue should be ~D." n))

(defun test-deque-size (deque-constructor &optional (count 1000))
  (let ((deque (funcall deque-constructor)))
    (assert (zerop (size deque)) () "Size of new deque should be zero.")
    (loop for i from 1 to count
          do (enqueue* deque i)
             (assert-queue-size deque i))
    (loop for i from (1- count) downto 0
          do (dequeue* deque)
             (assert-queue-size deque i))
    (assert (emptyp deque) () "Deque should be empty."))
  t)

(defun test-queue-clear (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) :count count)))
    (assert (not (emptyp queue)) () "Queue should have ~D elements." count)
    (clear queue)
    (assert (emptyp queue) () "Queue should be empty.")
    (assert-queue-size queue 0)
    (fill queue :count count)
    (assert (not (emptyp queue)) () "Emptying queue should not break it.")
    t))

(defun test-queue-elements (queue-constructor &optional (count 1000))
  (let* ((queue (fill (funcall queue-constructor) :count count))
         (expected (loop for i from 1 to count collect i))
         (elements (elements queue)))
    (assert (equal expected elements) () "FIFO elements should be ~A not ~A" (subseq expected 0 10) (subseq elements 0 10))
    (assert (emptyp queue) () "Mutable queue should be empty after elements are extracted."))
  t)
    
;;;
;;;    Not as straightforward as TEST-STACK-PUSH...
;;;    
(defun test-queue-enqueue (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (loop for i from 1 to count
          do (enqueue queue i)
             (let ((dequeued (dequeue queue)))
               (assert (= i dequeued) () "Wrong value enqueued: ~A should be: ~A~%" dequeued i))))
  t)

(defun test-queue-enqueue-wrong-type (queue-constructor)
  (let ((queue (funcall queue-constructor :type 'integer)))
    (handler-case (enqueue queue 1d0)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't ENQUEUE value of wrong type onto queue.~%"))))
  t)

(defun test-deque-enqueue* (deque-constructor &optional (count 1000))
  (let ((deque (funcall deque-constructor)))
    (loop for i from 1 to count
          do (enqueue* deque i)
             (let ((dequeued (dequeue* deque)))
               (assert (= i dequeued) () "Wrong value enqueued at rear: ~A should be: ~A~%" dequeued i))))
  t)

(defun test-deque-enqueue*-wrong-type (deque-constructor)
  (let ((deque (funcall deque-constructor :type 'integer)))
    (handler-case (enqueue* deque 1d0)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't ENQUEUE* value of wrong type onto deque.~%"))))
  t)

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
    (time
     (progn
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
         (dequeue queue))))
    (assert (emptyp queue)))
  t)

(deftest queue-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-queue-constructor
                 test-queue-emptyp
                 test-queue-size
                 test-queue-clear
                 test-queue-elements
                 test-queue-enqueue
                 test-queue-enqueue-wrong-type
                 test-queue-front-dequeue
                 test-queue-time
                 test-queue-wave)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest deque-test-suite (constructor)
  (queue-test-suite constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-deque-constructor
                 test-deque-emptyp
                 test-deque-size
                 test-deque-enqueue*
                 test-deque-enqueue*-wrong-type
                 test-deque-rear-dequeue*
                 test-deque-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-array-ring-buffer ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'array-ring-buffer :type type)))) )

(deftest test-linked-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'linked-queue :type type)))) )

(deftest test-array-list-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'array-list-queue :type type)))) )

(deftest test-linked-list-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'linked-list-queue :type type)))) )

(deftest test-dll-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'dll-queue :type type)))) )

(deftest test-circular-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'circular-queue :type type)))) )

(deftest test-recycling-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'recycling-queue :type type)))) )

(deftest test-linked-ring-buffer ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'linked-ring-buffer :type type)))) )

(deftest test-hash-table-queue ()
  (check
   (queue-test-suite #'(lambda (&key (type t)) (make-instance 'hash-table-queue :type type)))) )

(deftest test-array-ring-buffer-deque ()
  (check
   (deque-test-suite #'make-array-ring-buffer-deque)))

(deftest test-array-list-deque ()
  (check
   (deque-test-suite #'make-array-list-deque)))

(deftest test-linked-list-deque ()
  (check
   (deque-test-suite #'make-linked-list-deque)))

(deftest test-dll-deque ()
  (check
   (deque-test-suite #'make-dll-deque)))

(deftest test-hash-table-deque ()
  (check
   (deque-test-suite #'make-hash-table-deque)))

(deftest test-queue-all ()
  (check
   (test-array-ring-buffer)
   (test-linked-queue)
   (test-array-list-queue)
   (test-linked-list-queue)
   (test-dll-queue)
   (test-circular-queue)
   (test-recycling-queue)
   (test-linked-ring-buffer)
   (test-hash-table-queue)
   (test-array-ring-buffer-deque)
   (test-array-list-deque)
   (test-linked-list-deque)
   (test-dll-deque)
   (test-hash-table-deque)))
