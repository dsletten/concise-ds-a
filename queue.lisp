;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               queue.lisp
;;;;
;;;;   Started:            Sat Nov 13 14:14:14 2021
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

(in-package :containers)

;;;
;;;    QUEUE
;;;
(defclass queue (dispenser)
  ()
  (:documentation "A queue is a dispenser holding a sequence of elements that allows insertions only at one end (the rear) and deletions and access to elements at the other end (the front) (FIFO)."))

(defmethod clear ((q queue))
  (loop until (emptyp q) do (dequeue q)))

(defgeneric enqueue (queue obj)
  (:documentation "Enqueue an object at the rear of the queue"))
;; (defmethod enqueue :around ((q queue) obj)
;;   (with-slots (type) q
;;     (if (typep obj type)
;;         (call-next-method)
;;         (error "~A is not of type ~A" obj type))))
(defmethod enqueue :around ((q queue) obj)
  (if (typep obj (type q))
      (call-next-method)
      (error "~A is not of type ~A" obj (type q))))
(defmethod enqueue ((q queue) obj)
  (declare (ignore q obj))
  (error "queue does not implement ENQUEUE"))

;;;
;;;    DEQUEUE should ensure that reference to object being removed from queue is actually removed in order to
;;;    allow the object to be GC'd as eligible.
;;;    
(defgeneric dequeue (queue)
  (:documentation "Remove an object from the front of the queue"))
(defmethod dequeue :around ((q queue))
  (if (emptyp q)
      (error "Queue is empty")
      (call-next-method)))
(defmethod dequeue ((q queue))
  (declare (ignore q))
  (error "queue does not implement DEQUEUE"))

(defgeneric front (queue)
  (:documentation "Examine object at the front of the queue"))
(defmethod front :around ((q queue))
  (if (emptyp q)
      (error "Queue is empty")
      (call-next-method)))
(defmethod front ((q queue))
  (declare (ignore q))
  (error "queue does not implement FRONT"))

;;;
;;;    ARRAY-QUEUE - Uses "circular" array. As long as there is room, queue can
;;;    wrap around from end of array to start.
;;;    
(defconstant array-queue-capacity 20)

(defclass array-queue (queue)
  ((store)
   (front :initform 0) ; As long as queue is not empty FRONT is index of first queue elt.
   (count :initform 0)))

(defmethod initialize-instance :after ((q array-queue) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) q
    (setf store (make-array array-queue-capacity :element-type (type q)))) )

(defmethod size ((q array-queue))
  (with-slots (count) q
    count))

(defmethod emptyp ((q array-queue))
  (zerop (size q)))

;;;
;;;    This is not good enough. Must release the references to elements. Use superclass method.
;;;    
;; (defmethod clear ((q array-queue))
;;   (with-slots (front count) q
;;     (setf front 0
;;           count 0)))

;;;   Queue full?
;;;   2 approaches to a fixed-size (array) queue:
;;;   0. Go right ahead...Elements overwritten!!
;;;   1. When filled, signal error. Can't add any more...
;;;   2. Expand the array store. This requires realigning any "wrapped around" elements. No mechanism to ever shrink?
;;;   
;; (defmethod enqueue :around ((q array-queue) obj)
;;   (if (= (size q) array-queue-capacity)
;;       (error "Queue is full")
;;       (call-next-method)))
;; (defmethod enqueue :around ((q array-queue) obj) ; This is called before the QUEUE :AROUND method. We resize before checking whether type is allowed?!
;;   (with-slots (store) q
;;     (when (= (size q) (length store))
;;       (resize q))
;;     (call-next-method)))
(defmethod enqueue :before ((q array-queue) obj)
  (declare (ignore obj))
  (with-slots (store) q
    (when (= (size q) (length store))
      (resize q))))
(defmethod enqueue ((q array-queue) obj)
  (with-slots (store front count) q
    (setf (aref store (mod (+ front count) (length store))) obj)
    (incf count)))

;;;
;;;    Use SUBSEQ??
;;;    
(defun resize (q)
  (with-slots (store front count) q
    (let* ((length (length store))
           (new-store (make-array (* 2 length) :element-type (type q))))
      (dotimes (i count)
        (setf (aref new-store i) (aref store (mod (+ front i) length))))
      (setf store new-store
            front 0))))

(defmethod dequeue ((q array-queue))
  (with-slots (store front count) q
    (prog1 (front q)
      (setf (aref store front) nil
            front (mod (1+ front) (length store)))
      (decf count))))

(defmethod front ((q array-queue))
  (with-slots (store front) q
    (aref store front)))

;;;
;;;    LINKED-QUEUE
;;;    - This is a TCONC queue not merely a singly-linked list.
;;;    
(defclass linked-queue (queue)
  ((front :initform nil)
   (rear :initform nil)
   (count :initform 0)))

(defmethod size ((q linked-queue))
  (with-slots (count) q
    count))

(defmethod emptyp ((q linked-queue))
  (zerop (size q))) ; (null front)

(defmethod clear ((q linked-queue))
  (with-slots (front rear count) q
    (setf front nil
          rear nil
          count 0)))

(defmethod enqueue ((q linked-queue) obj)
  (with-slots (front rear count) q
    (cond ((null front) ; (emptyp q)
           (assert (null rear))
           (setf rear (setf front (cl:list obj))))
          (t (setf rear (setf (rest rear) (cl:list obj)))) )
    (incf count)))

(defmethod dequeue ((q linked-queue))
  (with-slots (front rear count) q
    (prog1 (front q)
      (setf front (rest front))
      (when (null front)
        (setf rear front))
      (decf count))))

(defmethod front ((q linked-queue))
  (with-slots (front) q
    (first front)))

;;;
;;;    CIRCULAR-QUEUE
;;;    See ch. 6 exercise 5
;;;    
(defclass circular-queue (queue)
  ((index :initform nil)
   (count :initform 0)))

(defmethod size ((q circular-queue))
  (with-slots (count) q
    count))

(defmethod emptyp ((q circular-queue))
  (zerop (size q)))

(defmethod clear ((q circular-queue))
  (with-slots (index count) q
    (setf index nil
          count 0)))

(defmethod enqueue ((q circular-queue) obj)
  (with-slots (index count) q
    (let ((node (cl:list obj)))
      (cond ((null index) (setf index node
                                (rest index) node))
            (t (setf (rest node) (rest index)
                     (rest index) node
                     index node))))
    (incf count)))

(defmethod dequeue ((q circular-queue))
  (with-slots (index count) q
    (prog1 (front q)
      (if (eq index (rest index))
          (setf index nil)
          (setf (rest index) (rest (rest index))))
      (decf count))))

(defmethod front ((q circular-queue))
  (with-slots (index) q
    (first (rest index))))

;;;
;;;    RECYCLING-QUEUE
;;;    - A linked queue that recycles CONS cells after DEQUEUEing to mimimize creating garbage.
;;;    
(defconstant linked-queue-capacity 20)

(defclass recycling-queue (linked-queue)
  ((ass)))

(defmethod initialize-instance :after ((q recycling-queue) &rest initargs)
  (declare (ignore initargs))
  (with-slots (front rear ass) q
    (setf front (make-list linked-queue-capacity)
          rear front
          ass (last front))))

(defmethod clear ((q recycling-queue))
  (loop until (emptyp q) do (dequeue q)))

(defmethod enqueue ((q recycling-queue) obj)
  (with-slots (front rear ass count) q
    (setf (car rear) obj)
    (when (eq rear ass)
      (let ((more (make-list (1+ count))))
        (setf (cdr ass) more
              ass (last more))))
    (setf rear (cdr rear))
    (incf count)))

;; (defmethod dequeue ((q recycling-queue))
;;   (with-slots (front rear ass count) q
;;     (prog1 (front q)
;;       (let ((next (cdr front)))
;;         (setf (cdr front) nil
;;               (car front) nil
;;               (cdr ass) front
;;               ass front
;;               front next))
;;       (decf count))))
(defmethod dequeue ((q recycling-queue))
  (with-slots (front rear ass count) q
    (prog1 (front q)
      (setf (cdr ass) front
            ass front
            front (cdr front)
            (car ass) nil
            (cdr ass) nil)
      (decf count))))

;;;
;;;    RING-BUFFER
;;;    
(defclass ring-buffer (linked-queue)
  ()
  (:documentation "Queue is a circular list. Ring grows as necessary."))

(defmethod initialize-instance :after ((q ring-buffer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (front rear) q
    (setf front (make-list linked-queue-capacity)
          rear front
          (cdr (last front)) front)))

;;;
;;;    Can't skip parent method to get to grandparent method?!?
;;;    
(defmethod clear ((q ring-buffer))
  (loop until (emptyp q) do (dequeue q)))

;; (defmethod enqueue ((q ring-buffer) obj)
;;   (with-slots (front rear count) q
;;     (setf (car rear) obj)
;;     (when (eq (cdr rear) front)
;;       (let ((more (make-list (1+ count))))
;;         (setf (cdr rear) more
;;               (cdr (last more)) front)))
;;     (setf rear (cdr rear))
;;     (incf count)))

(defmethod enqueue ((q ring-buffer) obj)
  (with-slots (front rear count) q
    (setf (car rear) obj)
    (when (eq (cdr rear) front)
      (setf (cdr rear) (nconc (make-list (1+ count)) front)))
    (setf rear (cdr rear))
    (incf count)))

(defmethod dequeue ((q ring-buffer))
  (with-slots (front rear count) q
    (prog1 (front q)
      (setf (car front) nil
            front (cdr front))
      (decf count))))

;;;
;;;    HASH-TABLE-QUEUE
;;;    - Hash-table queue allows "rewinding" elements that have been dequeued if REMHASH is omitted!!
;;;      Or chain 2 queues? A queue and a stack?
;;;    - Also supports "rolling back" last enqueue. (Compare ROLLBACK-QUEUE in collections.lisp)
;;;
(defclass hash-table-queue (queue)
  ((store :initform (make-hash-table))
   (front :initform 0)
   (rear :initform 0)))

(defmethod size ((q hash-table-queue))
  (with-slots (store) q
    (hash-table-count store)))

(defmethod emptyp ((q hash-table-queue))
  (zerop (size q)))

(defmethod clear ((q hash-table-queue))
  (with-slots (store front rear) q
    (clrhash store)
    (setf front 0
          rear 0)))

(defmethod enqueue ((q hash-table-queue) obj)
  (with-slots (store rear) q
    (setf (gethash rear store) obj)
    (incf rear)))

(defmethod dequeue ((q hash-table-queue))
  (with-slots (store front) q
    (prog1 (front q)
      (remhash front store)
      (incf front))))

(defmethod front ((q hash-table-queue))
  (with-slots (store front) q
    (values (gethash front store))))

;;;
;;;    PERSISTENT-QUEUE (Linked queue)
;;;    - Invariant: Whenever queue is not empty, front list must be non-empty.
;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-QUEUE...
;;;      
(defclass persistent-queue (queue)
  ((front :initform '() :initarg :front)
   (rear :initform '() :initarg :rear)
   (count :initform 0 :initarg :count)))

(defmethod size ((q persistent-queue))
  (with-slots (count) q
    count))

(defmethod emptyp ((q persistent-queue))
  (zerop (size q)))

;; (defmethod clear ((q persistent-queue))
;;   (with-slots (type) q
;;     (make-instance 'persistent-queue :type type)))
(defmethod clear ((q persistent-queue))
  (make-instance 'persistent-queue :type (type q)))

(defmethod enqueue ((q persistent-queue) obj)
  (with-slots (type front rear count) q
    (if (emptyp q)
        (make-instance 'persistent-queue :type type :front (cl:list obj) :count 1)
        (make-instance 'persistent-queue :type type :front front :rear (cons obj rear) :count (1+ count)))) )

(defmethod dequeue ((q persistent-queue))
  (with-slots (type front rear count) q
    (if (null (rest front)) ; Could check for single-elt REAR...
        (values (make-instance 'persistent-queue :type type :front (reverse rear) :rear '() :count (1- count)) (front q))
        (values (make-instance 'persistent-queue :type type :front (rest front) :rear rear :count (1- count)) (front q)))) )

(defmethod front ((q persistent-queue))
  (with-slots (front) q
    (first front)))

;;;
;;;    Enqueue list into PERSISTENT-QUEUE
;;;    
(defun enqueue-all (q elts)
  (reduce #'(lambda (q elt) (enqueue q elt)) elts :initial-value q))

(defun dequeue-all (q)
  (unless (emptyp q)
    (loop for (queue elt) = (multiple-value-list (dequeue q)) then (multiple-value-list (dequeue queue))
          until (emptyp queue)
          do (print elt)
          finally (print elt))))

;;;
;;;    DEQUE (Double-ended queue)
;;;
(defclass deque (queue) ; Inherited error msgs???
  ()
  (:documentation "A deque is a double-ended queue allowing ENQUEUE and DEQUEUE operations on both the front and rear."))

(defmethod emptyp ((dq deque))
  (zerop (size dq)))

(defgeneric enqueue* (deque obj)
  (:documentation "Enqueue an object at the front of the deque"))
(defmethod enqueue* :around ((dq deque) obj)
  (if (typep obj (type dq))
      (call-next-method)
      (error "~A is not of type ~A" obj (type dq))))
(defmethod enqueue* ((dq deque) obj)
  (declare (ignore dq obj))
  (error "deque does not implement ENQUEUE*"))

(defgeneric dequeue* (deque)
  (:documentation "Remove an object from the rear of the deque"))
(defmethod dequeue* :around ((dq deque))
  (if (emptyp dq)
      (error "Deque is empty")
      (call-next-method)))
(defmethod dequeue* ((dq deque))
  (declare (ignore dq))
  (error "deque does not implement DEQUEUE*"))

(defgeneric rear (deque)
  (:documentation "Examine object at the rear of the deque"))
(defmethod rear :around ((dq deque))
  (if (emptyp dq)
      (error "Deque is empty")
      (call-next-method)))
(defmethod rear ((dq deque))
  (declare (ignore dq))
  (error "deque does not implement REAR"))

;;;
;;;    Doubly-linked-list deque
;;;    
(defclass dll-deque (deque)
  ((list :initarg :list)))

(defun make-dll-deque (&optional (type t))
  (make-instance 'dll-deque :type type :list (make-doubly-linked-list :type type))) ; ??
;  (make-instance 'dll-deque :type type :list (make-doubly-linked-list))) ; ??

(defmethod size ((dq dll-deque))
  (with-slots (list) dq
    (size list)))

;; (defmethod clear ((dq dll-deque))
;;   (with-slots (list) dq
;;     (clear list)))

(defmethod enqueue ((dq dll-deque) obj)
  (with-slots (list) dq
    (add list obj)))

(defmethod dequeue ((dq dll-deque))
  (with-slots (list) dq
    (prog1 (front dq)
      (delete list 0))))

(defmethod enqueue* ((dq dll-deque) obj)
  (with-slots (list) dq
    (insert list 0 obj)))

(defmethod dequeue* ((dq dll-deque))
  (with-slots (list) dq
    (prog1 (rear dq)
      (delete list -1))))

(defmethod front ((dq dll-deque))
  (with-slots (list) dq
    (nth list 0)))

(defmethod rear ((dq dll-deque))
  (with-slots (list) dq
    (nth list -1)))

;;;
;;;   HASH-TABLE-DEQUE
;;;
(defclass hash-table-deque (deque)
  ((store :initform (make-hash-table))
   (front :initform 0)
   (rear :initform 0)))

(defun make-hash-table-deque (&optional (type t))
  (make-instance 'hash-table-deque :type type))

(defmethod size ((dq hash-table-deque))
  (with-slots (store) dq
    (hash-table-count store)))

;; (defmethod emptyp ((dq hash-table-deque))
;;   (zerop (size dq)))

(defmethod clear ((dq hash-table-deque))
  (with-slots (store front rear) dq
    (clrhash store)
    (setf front 0
          rear 0)))

(defmethod enqueue :before ((dq hash-table-deque) obj)
  (declare (ignore obj))
  (with-slots (rear) dq
    (unless (emptyp dq)
      (incf rear))))
(defmethod enqueue ((dq hash-table-deque) obj)
  (with-slots (store rear) dq
    (setf (gethash rear store) obj)))

(defmethod dequeue ((dq hash-table-deque))
  (with-slots (store front) dq
    (prog1 (front dq)
      (remhash front store))))
(defmethod dequeue :after ((dq hash-table-deque))
  (with-slots (front) dq
    (unless (emptyp dq)
      (incf front))))

(defmethod enqueue* :before ((dq hash-table-deque) obj)
  (declare (ignore obj))
  (with-slots (front) dq
    (unless (emptyp dq)
      (decf front))))
(defmethod enqueue* ((dq hash-table-deque) obj)
  (with-slots (store front) dq
    (setf (gethash front store) obj)))

(defmethod dequeue* ((dq hash-table-deque))
  (with-slots (store rear) dq
    (prog1 (rear dq)
      (remhash rear store))))
(defmethod dequeue* :after ((dq hash-table-deque))
  (with-slots (rear) dq
    (unless (emptyp dq)
      (decf rear))))

(defmethod front ((dq hash-table-deque))
  (with-slots (store front) dq
    (values (gethash front store))))

(defmethod rear ((dq hash-table-deque))
  (with-slots (store rear) dq
    (values (gethash rear store))))
