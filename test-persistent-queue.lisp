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

(defmethod fill ((queue persistent-queue) &key (count 1000) (generator #'identity))
  (labels ((fill-er-up (q i)
             (if (> i count)
                 q
                 (fill-er-up (enqueue q (funcall generator i)) (1+ i)))) )
    (fill-er-up queue 1)))

;; (defmethod fill ((queue persistent-queue) &optional (count 1000))
;;   (do* ((i 1 (1+ i))
;;         (new-queue (enqueue queue i) (enqueue new-queue i)))
;;        ((= i count) new-queue)))
;;   ;; (loop for i from 1 to count
;;   ;;       for new-queue = (enqueue queue i) then (enqueue new-queue i)   ; ??????? Scope problem without renaming?!??!
;;   ;;       finally (return new-queue)))

;;;
;;;    See test-persistent-stack.lisp
;;;    
(defmethod fill ((queue persistent-list-queue) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-queue = (enqueue queue (funcall generator i)) then (enqueue new-queue (funcall generator i))
        finally (return new-queue)))

(defun test-persistent-queue-constructor (queue-constructor)
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

(defun test-persistent-deque-constructor (deque-constructor)
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

(defun test-persistent-queue-emptyp (queue-constructor)
  (let ((queue (funcall queue-constructor)))
    (assert (emptyp queue) () "New queue should be empty.")
    (assert (not (emptyp (enqueue queue t))) () "Queue with elt should not be empty.")
    (assert (emptyp (dequeue (enqueue queue t))) () "Empty queue should be empty.")
    t))

(defun test-persistent-deque-emptyp (deque-constructor)
  (let ((deque (funcall deque-constructor)))
    (assert (emptyp deque) () "New deque should be empty.")
    (assert (not (emptyp (enqueue* deque t))) () "Deque with elt should not be empty.")
    (assert (emptyp (dequeue* (enqueue* deque t))) () "Empty deque should be empty.")
    t))

(defun test-persistent-queue-size (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (assert (zerop (size queue)) () "Size of new queue should be zero.")
    (loop for i from 1 to count
          for new-queue = (enqueue queue i) then (enqueue new-queue i)   ; ??????? Scope problem without renaming?!??!
          do (assert (= (size new-queue) i) () "Size of queue should be ~D." i)
          finally (return t))))

(defun test-persistent-deque-size (deque-constructor &optional (count 1000))
  (let ((deque (funcall deque-constructor)))
    (assert (zerop (size deque)) () "Size of new deque should be zero.")
    (loop for i from 1 to count
          for new-deque = (enqueue* deque i) then (enqueue* new-deque i)   ; ??????? Scope problem without renaming?!??!
          do (assert (= (size new-deque) i) () "Size of deque should be ~D." i)
          finally (return t))))

(defun test-persistent-queue-clear (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) :count count)))
    (assert (not (emptyp queue)) () "Queue should have ~D elements." count)
    (assert (emptyp (clear queue)) () "Queue should be empty.")
    (assert (zerop (size (clear queue))) () "Size of queue should be 0."))
  t)

(defun test-persistent-queue-front-dequeue (queue-constructor &optional (count 1000))
  (let ((queue (fill (funcall queue-constructor) :count count)))
    (loop repeat (size queue)
          for front = (front queue)
          do (multiple-value-bind (q dequeued) (dequeue queue)
               (setf queue q)
               (assert (= front dequeued) () "Wrong value dequeued: ~A should be: ~A~%" dequeued front)))
    (assert (emptyp queue) () "Queue should be empty."))
  t)

(defun test-persistent-deque-rear-dequeue* (deque-constructor &optional (count 1000))
  (let ((deque (fill (funcall deque-constructor) :count count)))
    (loop repeat (size deque)
          for rear = (rear deque)
          do (multiple-value-bind (d dequeued) (dequeue* deque)
               (setf deque d)
               (assert (= rear dequeued) () "Wrong value dequeued from rear: ~A should be: ~A~%" dequeued rear)))
    (assert (emptyp deque) () "Deque should be empty."))
  t)

;(defun test-persistent-queue-time (queue-constructor &optional (count 100000))
(defun test-persistent-queue-time (queue-constructor &optional (count 1000))
  (let ((queue (funcall queue-constructor)))
    (time
     (dotimes (i 10 t)
       (loop for new-queue = (fill queue :count count) then (dequeue new-queue)
             until (emptyp new-queue)))) ))

(defun test-persistent-deque-time (deque-constructor &optional (count 1000))
  (let ((deque (funcall deque-constructor)))
    (time
     (dotimes (i 10 t)
       (dotimes (j count)
         (let ((deque (loop for i from 1 to count
                            for new-deque = (enqueue* deque i) then (enqueue* new-deque i)
                            finally (return new-deque))))
           (loop for new-deque = (dequeue* deque) then (dequeue* new-deque)
                 until (emptyp new-deque)))) ))))

(deftest persistent-queue-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-persistent-queue-constructor
                 test-persistent-queue-emptyp
                 test-persistent-queue-size
                 test-persistent-queue-clear
                 test-persistent-queue-front-dequeue
                 test-persistent-queue-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest persistent-deque-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-persistent-queue-constructor
                 test-persistent-deque-constructor
                 test-persistent-queue-emptyp
                 test-persistent-deque-emptyp
                 test-persistent-queue-size
                 test-persistent-deque-size
                 test-persistent-queue-clear
                 test-persistent-queue-front-dequeue
                 test-persistent-deque-rear-dequeue*
                 test-persistent-queue-time
                 test-persistent-deque-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-persistent-queue ()
  (check
   (persistent-queue-test-suite #'(lambda () (make-instance 'persistent-queue)))) )

(deftest test-persistent-list-queue ()
  (check
   (persistent-queue-test-suite #'(lambda () (make-instance 'persistent-list-queue)))) )

(deftest test-persistent-deque ()
  (check
   (persistent-deque-test-suite #'(lambda () (make-instance 'persistent-deque)))) )

(deftest test-persistent-list-deque ()
  (check
   (persistent-deque-test-suite #'(lambda () (make-instance 'persistent-list-deque)))) )

(deftest test-persistent-queue-all ()
  (check
   (test-persistent-queue)
   (test-persistent-list-queue)
   (test-persistent-deque)
   (test-persistent-list-deque)))
   
;; * (time (fill (make-instance 'persistent-list-queue) 100))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000395 seconds of total run time (0.000395 user, 0.000000 system)
;;   100.00% CPU
;;   1,406,878 processor cycles
;;   98,304 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {100352E873}>
;; * (time (fill (make-instance 'persistent-list-queue) 1000))
;; Evaluation took:
;;   0.012 seconds of real time
;;   0.013032 seconds of total run time (0.013032 user, 0.000000 system)
;;   108.33% CPU
;;   46,896,168 processor cycles
;;   8,224,400 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {1003F4B213}>
;; * (time (fill (make-instance 'persistent-list-queue) 10000))
;; Evaluation took:
;;   0.208 seconds of real time
;;   0.207906 seconds of total run time (0.200291 user, 0.007615 system)
;;   [ Run times consist of 0.030 seconds GC time, and 0.178 seconds non-GC time. ]
;;   100.00% CPU
;;   746,037,930 processor cycles
;;   802,311,440 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {10037F7F63}>
;; * (time (fill (make-instance 'persistent-list-queue) 100000))
;; Evaluation took:
;;   21.884 seconds of real time
;;   21.918248 seconds of total run time (19.748664 user, 2.169584 system)
;;   [ Run times consist of 4.482 seconds GC time, and 17.437 seconds non-GC time. ]
;;   100.16% CPU
;;   78,774,902,488 processor cycles
;;   80,023,201,776 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {100311A8D3}>
;; * (time (fill (make-instance 'persistent-queue) 100))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000026 seconds of total run time (0.000023 user, 0.000003 system)
;;   100.00% CPU
;;   90,120 processor cycles
;;   0 bytes consed
  
;; #<PERSISTENT-QUEUE {1003147A23}>
;; * (time (fill (make-instance 'persistent-queue) 1000))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000632 seconds of total run time (0.000632 user, 0.000000 system)
;;   100.00% CPU
;;   2,262,596 processor cycles
;;   81,888 bytes consed
  
;; #<PERSISTENT-QUEUE {10033A5F93}>
;; * (time (fill (make-instance 'persistent-queue) 10000))
;; Evaluation took:
;;   0.008 seconds of real time
;;   0.006831 seconds of total run time (0.006831 user, 0.000000 system)
;;   87.50% CPU
;;   24,579,194 processor cycles
;;   802,304 bytes consed
  
;; #<PERSISTENT-QUEUE {10037781D3}>
;; * (time (fill (make-instance 'persistent-queue) 100000))
;; Evaluation took:
;;   0.048 seconds of real time
;;   0.046804 seconds of total run time (0.046804 user, 0.000000 system)
;;   97.92% CPU
;;   168,480,050 processor cycles
;;   8,006,592 bytes consed
  
;; #<PERSISTENT-QUEUE {1003F653C3}>

;; ;;;
;; ;;;    Memory consumption grows quadratically
;; ;;;    
;; * (time (fill (make-instance 'persistent-list-queue) 1000))
;; Evaluation took:
;;   0.004 seconds of real time
;;   0.004106 seconds of total run time (0.004098 user, 0.000008 system)
;;   100.00% CPU
;;   14,771,714 processor cycles
;;   8,240,976 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {1004761C23}>
;; * (time (fill (make-instance 'persistent-list-queue) 2000))
;; Evaluation took:
;;   0.012 seconds of real time
;;   0.009276 seconds of total run time (0.009276 user, 0.000000 system)
;;   75.00% CPU
;;   33,416,396 processor cycles
;;   32,455,920 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {10066A6983}>
;; * (time (fill (make-instance 'persistent-list-queue) 4000))
;; Evaluation took:
;;   0.051 seconds of real time
;;   0.051722 seconds of total run time (0.051722 user, 0.000000 system)
;;   [ Run times consist of 0.004 seconds GC time, and 0.048 seconds non-GC time. ]
;;   101.96% CPU
;;   185,865,514 processor cycles
;;   128,928,464 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {10047E06A3}>
;; * (time (fill (make-instance 'persistent-list-queue) 8000))
;; Evaluation took:
;;   0.163 seconds of real time
;;   0.162021 seconds of total run time (0.157897 user, 0.004124 system)
;;   [ Run times consist of 0.017 seconds GC time, and 0.146 seconds non-GC time. ]
;;   99.39% CPU
;;   584,007,800 processor cycles
;;   513,855,088 bytes consed
  
;; #<PERSISTENT-LIST-QUEUE {1006553BA3}>

;;;
;;;    The main inefficiency is the ENQUEUE method since the PERSISTENT-LIST must be traversed and copied!!
;;;    Adding N elements to a queue of M elements:
;;;    M + 1
;;;    M + 2
;;;    ...
;;;    M + N
;;;   -------
;;;   NM + N(N+1)/2
;;;
;; * (let ((queue (fill (make-instance 'persistent-queue) 10000)))
;;   (time (loop for new-queue = queue then (dequeue new-queue)
;;               until (emptyp new-queue))))

;; Evaluation took:
;;   0.004 seconds of real time
;;   0.003345 seconds of total run time (0.003345 user, 0.000000 system)
;;   [ Run times consist of 0.001 seconds GC time, and 0.003 seconds non-GC time. ]
;;   75.00% CPU
;;   11,887,912 processor cycles
;;   785,824 bytes consed
  
;; NIL
;; * (let ((queue (fill (make-instance 'persistent-list-queue) 10000)))
;;   (time (loop for new-queue = queue then (dequeue new-queue)
;;               until (emptyp new-queue))))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.002643 seconds of total run time (0.002398 user, 0.000245 system)
;;   100.00% CPU
;;   9,515,494 processor cycles
;;   1,909,616 bytes consed
  
;; NIL
