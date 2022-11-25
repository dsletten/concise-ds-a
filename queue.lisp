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
;;;;   - ARRAY-QUEUE is the traditional ring buffer. (另见 (ADJUSTABLE-)CIRCULAR-ARRAY-QUEUE in "foundations")
;;;;   - CIRCULAR-QUEUE, RECYCLING-QUEUE, and RING-BUFFER are just variants of LINKED-QUEUE:
;;;;     (RECYCLING-QUEUE and RING-BUFFER require inapproriate access to slots in LINKED-QUEUE)???
;;;;     - CIRCULAR-QUEUE grows/shrinks with each operation.
;;;;     - RECYCLING-QUEUE/RING-BUFFER start pre-allocated, overwrite existing CONSes, grow as needed and never shrink.
;;;;       - At first glance, RECYCLING-QUEUE appears to be an interesting variation. In fact, it is only trivially different from RING-BUFFER
;;;;         and requires 3 extra SETFs for each DEQUEUE. ENQUEUE is similar for each.
;;;;
;;;;         Effectively, RECYCLING-QUEUE's DEQUEUE unlinks the previous `front` of the queue and links it to the end so that the
;;;;         CONS cell is recycled.
;;;;         Before
;;;;         甲[*|*]--->[*|*]--->[*|*]---> ... --->[*|*]--->NIL
;;;;            |        |        |                 |
;;;;            v        v        v                 v
;;;;            8        9        10               NIL
;;;;         After
;;;;         [*|*]--->[*|*]---> ... --->[*|*]--->甲[*|*]--->NIL
;;;;          |        |                 |          |
;;;;          v        v                 v          v
;;;;          9        10               NIL        NIL
;;;;          
;;;;         By contrast, RING-BUFFER is a circular list, so the FRONT pointer just moves along as needed after each DEQUEUE.
;;;;         Before
;;;;         #1=[*|*]--->[*|*]--->[*|*]---> ... --->[*|*]--->#1#
;;;;             |        |        |                 |
;;;;             v        v        v                 v
;;;;             8        9        10               NIL
;;;;         #1=[*|*]--->[*|*]---> ... --->[*|*]--->[*|*]--->#1#
;;;;             |        |                 |        |
;;;;             v        v                 v        v
;;;;             9        10               NIL      NIL
;;;;
;;;;
;;;;
;;;;    Fox does not emphasize how a list can be used to implement a queue.
;;;;    见 LINKED-LIST-QUEUE, PERSISTENT-LIST-QUEUE
;;;;    It is reasonable to implement a linked queue using SINGLY-LINKED-LIST-X.
;;;;    It is not reasonable to implement a persistent queue using PERSISTENT-LIST!
;;;;    ENQUEUE is way too inefficient. 见 test cases.
;;;;
;;;;    DEQUE inherits QUEUE error messages?!
;;;;


(in-package :containers)

;;;
;;;    QUEUE
;;;
(defclass queue (dispenser)
  ()
  (:documentation "A queue is a dispenser holding a sequence of elements that allows insertions only at one end (the rear) and deletions and access to elements at the other end (the front) (FIFO)."))

(defmethod emptyp ((q queue))
  (zerop (size q)))

(defmethod clear ((q queue))
  (loop until (emptyp q) do (dequeue q)))

(defgeneric enqueue (queue obj)
  (:documentation "Enqueue an object at the rear of the queue"))
(defmethod enqueue :around ((q queue) obj)
  (if (typep obj (type q))
      (call-next-method)
      (error "~A is not of type ~A" obj (type q))))
(defmethod enqueue ((q queue) obj)
  (declare (ignore q obj))
  (error "QUEUE does not implement ENQUEUE"))

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
  (error "QUEUE does not implement DEQUEUE"))

(defgeneric front (queue)
  (:documentation "Examine object at the front of the queue"))
(defmethod front :around ((q queue))
  (if (emptyp q)
      (error "Queue is empty")
      (call-next-method)))
(defmethod front ((q queue))
  (declare (ignore q))
  (error "QUEUE does not implement FRONT"))

;;;
;;;    ARRAY-QUEUE - Uses "circular" array. As long as there is room, queue can
;;;    wrap around from end of array to start.
;;;    - Ring buffer
;;;    - Never shrinks?!
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

;; (defmethod emptyp ((q array-queue))
;;   (zerop (size q)))

;;;
;;;    This is not good enough. Must release the references to elements. Use superclass method.
;;;    
;; (defmethod clear ((q array-queue))
;;   (with-slots (front count) q
;;     (setf front 0
;;           count 0)))

;;;   Queue full?
;;;   2 approaches to a fixed-size (array) circular queue:
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

;; (defmethod emptyp ((q linked-queue))
;;   (zerop (size q))) ; (null front)

(defmethod clear ((q linked-queue))
  (with-slots (front rear count) q
    (setf front nil
          rear nil
          count 0)))

(defmethod enqueue ((q linked-queue) obj)
  (with-slots (front rear count) q
    (let ((node (cl:list obj)))
      (cond ((emptyp q)
             (assert (null rear))
             (setf rear (setf front node)))
            (t (setf rear (setf (rest rear) node)))) )
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
;;;    - This just obviates the need for two pointers in LINKED-QUEUE.
;;;      INDEX points to tail of queue, and its CDR points to front of queue.
;;;    
(defclass circular-queue (queue)
  ((index :initform nil)
   (count :initform 0)))

(defmethod size ((q circular-queue))
  (with-slots (count) q
    count))

;; (defmethod emptyp ((q circular-queue))
;;   (zerop (size q)))

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

;;;
;;;    Override parent method -> grandparent!!
;;;    
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

;; (defmethod emptyp ((q hash-table-queue))
;;   (zerop (size q)))

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
;;;      - Similar issues to PERSISTENT-STACK
;;;      
(defclass persistent-queue (queue)
  ((front :initform '())
   (rear :initform '())
   (count :initform 0 :type integer)))

(defgeneric make-empty-persistent-queue (q)
  (:documentation "Create an empty persistent queue of a given element type."))
(defmethod make-empty-persistent-queue ((q persistent-queue))
  (make-instance 'persistent-queue :type (type q)))

(defmethod size ((q persistent-queue))
  (with-slots (count) q
    count))

;; (defmethod emptyp ((q persistent-queue))
;;   (zerop (size q)))

(defmethod clear ((q persistent-queue))
;  (make-instance 'persistent-queue :type (type q)))
  (make-empty-persistent-queue q))

(defun initialize-queue (q front rear count)
  (let ((new-queue (make-empty-persistent-queue q)))
    (with-slots ((new-front front) (new-rear rear) (new-count count)) new-queue
      (setf new-front front
            new-rear rear
            new-count count))
    new-queue))

(defmethod enqueue ((q persistent-queue) obj)
  (with-slots (front rear count) q
    (if (emptyp q)
        (initialize-queue q (cl:list obj) '() 1)
        (initialize-queue q front (cons obj rear) (1+ count)))) )

(defmethod dequeue ((q persistent-queue))
  (with-slots (front rear count) q
    (if (null (rest front))
        (values (initialize-queue q (cl:reverse rear) '() (1- count)) (front q))
        (values (initialize-queue q (rest front) rear (1- count)) (front q)))) )

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
