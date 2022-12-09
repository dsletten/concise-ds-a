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
;;;;   - ARRAY-RING-BUFFER is the traditional ring buffer. (另见 (ADJUSTABLE-)CIRCULAR-ARRAY-QUEUE in "foundations")
;;;;   - CIRCULAR-QUEUE, RECYCLING-QUEUE, and LINKED-RING-BUFFER are just variants of LINKED-QUEUE:
;;;;     (RECYCLING-QUEUE and LINKED-RING-BUFFER require inapproriate access to slots in LINKED-QUEUE)???
;;;;     - CIRCULAR-QUEUE grows/shrinks with each operation.
;;;;     - RECYCLING-QUEUE/LINKED-RING-BUFFER start pre-allocated, overwrite existing CONSes, grow as needed and never shrink.
;;;;       - At first glance, RECYCLING-QUEUE appears to be an interesting variation. In fact, it is only trivially different from LINKED-RING-BUFFER
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
;;;;         By contrast, LINKED-RING-BUFFER is a circular list, so the FRONT pointer just moves along as needed after each DEQUEUE.
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

(defmethod fill ((queue queue) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        do (enqueue queue (funcall generator i))
        finally (return queue)))

;; (defmethod fill ((deque deque) &optional (count 1000))
;;   (loop for i from 1 to count
;;         do (enqueue* deque i)
;;         finally (return deque)))

(defmethod elements ((queue queue))
  (loop until (emptyp queue)
        collect (dequeue queue)))

(defgeneric resize (queue)
  (:documentation "Resize the queue when it is full.")) ; Shrink??

;;;
;;;    ARRAY-RING-BUFFER - Uses "circular" array. As long as there is room, queue can
;;;    wrap around from end of array to start.
;;;    - Never shrinks?!
;;;    - CLEAR -> resize STORE??
;;;    
(defconstant array-ring-buffer-capacity 20)

;(defclass array-queue (queue)
(defclass array-ring-buffer (queue)
  ((store)
   (front :initform 0) ; As long as queue is not empty FRONT is index of first queue elt.
   (count :initform 0)))

(defmethod initialize-instance :after ((q array-ring-buffer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) q
    (setf store (make-array array-ring-buffer-capacity :element-type (type q)))) )

(defmethod size ((q array-ring-buffer))
  (with-slots (count) q
    count))

;;;
;;;    This is not good enough. Must release the references to elements. Use superclass method.
;;;    
;; (defmethod clear ((q array-ring-buffer))
;;   (with-slots (front count) q
;;     (setf front 0
;;           count 0)))

;;;   Queue full?
;;;   2 approaches to a fixed-size (array) circular queue:
;;;   0. Go right ahead...Elements overwritten!!
;;;   1. When filled, signal error. Can't add any more...
;;;   2. Expand the array store. This requires realigning any "wrapped around" elements. No mechanism to ever shrink?
;;;   
;; (defmethod enqueue :around ((q array-ring-buffer) obj)
;;   (if (= (size q) array-ring-buffer-capacity)
;;       (error "Queue is full")
;;       (call-next-method)))
;; (defmethod enqueue :around ((q array-ring-buffer) obj) ; This is called before the QUEUE :AROUND method. We resize before checking whether type is allowed?!
;;   (with-slots (store) q
;;     (when (= (size q) (length store))
;;       (resize q))
;;     (call-next-method)))

;;;
;;;    Use SUBSEQ??
;;;    
;; (flet ((offset (q i)
;;          (with-slots (store front) q
;;            (mod (+ front i) (length store)))) )
;;   (defmethod enqueue :before ((q array-ring-buffer) obj)
;;     (declare (ignore obj))
;;     (with-slots (store front count) q
;;       (let ((length (length store)))
;;         (flet ((resize ()
;;                  (let ((new-store (make-array (* 2 length) :element-type (type q))))
;;                    (dotimes (i count)
;;                      (setf (aref new-store i) (aref store (offset q i))))
;;                    (setf store new-store
;;                          front 0))))
;;           (when (= count length)
;;             (resize)))) ))
;;   (defmethod enqueue ((q array-ring-buffer) obj)
;;     (with-slots (store count) q
;;       (setf (aref store (offset q count)) obj)
;;       (incf count)))
;;   (defmethod dequeue ((q array-ring-buffer))
;;     (with-slots (store front count) q
;;       (prog1 (front q)
;;         (setf (aref store front) nil
;;               front (offset q 1))
;;         (decf count)))) )

;; (defmethod front ((q array-ring-buffer))
;;   (with-slots (store front) q
;;     (aref store front)))

(defun offset (q i) ; ARRAY-RING-BUFFER-OFFSET?
  (with-slots (store front) q
    (mod (+ front i) (length store))))

(defmethod resize ((q array-ring-buffer))
  (with-slots (front store count) q
    (assert (= count (length store)) () "RESIZE called without full STORE.")
    (let ((new-store (make-array (* 2 count) :element-type (type q))))
      (dotimes (i count)
        (setf (aref new-store i) (aref store (offset q i))))
      (setf store new-store
            front 0))))

(defmethod enqueue :before ((q array-ring-buffer) obj)
  (declare (ignore obj))
  (with-slots (store count) q
    (when (= count (length store))
      (resize q))))
(defmethod enqueue ((q array-ring-buffer) obj)
  (with-slots (store count) q
    (setf (aref store (offset q count)) obj)
    (incf count)))

(defmethod dequeue ((q array-ring-buffer))
  (with-slots (store front count) q
    (prog1 (front q)
      (setf (aref store front) nil
            front (offset q 1))
      (decf count))))

(defmethod front ((q array-ring-buffer))
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
  (with-slots (rear ass count) q
    (setf (first rear) obj)
    (when (eq rear ass)
      (let ((more (make-list (1+ count))))
        (setf (rest ass) more
              ass (last more))))
    (setf rear (rest rear))
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
  (with-slots (front ass count) q
    (prog1 (front q)
      (setf (rest ass) front
            ass front
            front (rest front)
            (first ass) nil
            (rest ass) nil)
      (decf count))))

;;;
;;;    LINKED-RING-BUFFER
;;;    
(defclass linked-ring-buffer (linked-queue)
  ()
  (:documentation "Queue is a circular list. Ring grows as necessary."))

(defmethod initialize-instance :after ((q linked-ring-buffer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (front rear) q
    (setf front (make-list linked-queue-capacity)
          rear front
          (rest (last front)) front)))

;;;
;;;    Can't skip parent method to get to grandparent method?!?
;;;    
(defmethod clear ((q linked-ring-buffer))
  (loop until (emptyp q) do (dequeue q)))

(defmethod resize ((q linked-ring-buffer))
  (with-slots (front rear count) q
    (assert (eq (rest rear) front) () "RESIZE called without full STORE.")
    (setf (rest rear) (nconc (make-list (1+ count)) front))))

(defmethod enqueue :before ((q linked-ring-buffer) obj)
  (declare (ignore obj))
  (with-slots (front rear) q
    (when (eq (rest rear) front)
      (resize q))))
(defmethod enqueue ((q linked-ring-buffer) obj)
  (with-slots (front rear count) q
    (setf (first rear) obj
          rear (rest rear))
    (incf count)))

(defmethod dequeue ((q linked-ring-buffer))
  (with-slots (front count) q
    (prog1 (front q)
      (setf (first front) nil
            front (rest front))
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
  ()
  (:documentation "A queue that defines non-destructive operations."))

(defmethod clear ((q persistent-queue))
  (make-empty-persistent-queue q))

(defmethod fill ((queue persistent-queue) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-queue = (enqueue queue (funcall generator i)) then (enqueue new-queue (funcall generator i))
        finally (return new-queue)))

;; (defmethod fill ((queue persistent-queue) &key (count 1000) (generator #'identity))
;;   (labels ((fill-er-up (q i)
;;              (if (> i count)
;;                  q
;;                  (fill-er-up (enqueue q (funcall generator i)) (1+ i)))) )
;;     (fill-er-up queue 1)))

;; (defmethod fill ((queue persistent-queue) &optional (count 1000))
;;   (do* ((i 1 (1+ i))
;;         (new-queue (enqueue queue i) (enqueue new-queue i)))
;;        ((= i count) new-queue)))
;;   ;; (loop for i from 1 to count
;;   ;;       for new-queue = (enqueue queue i) then (enqueue new-queue i)   ; ??????? Scope problem without renaming?!??!
;;   ;;       finally (return new-queue)))

(defmethod elements ((queue persistent-queue))
  (loop for new-queue = queue then (dequeue new-queue)
        until (emptyp new-queue)
        collect (front new-queue)))

(defgeneric make-empty-persistent-queue (q)
  (:documentation "Create an empty persistent queue of a given element type."))
(defmethod make-empty-persistent-queue ((q persistent-queue))
  (declare (ignore q))
  (error "PERSISTENT-QUEUE does not implement MAKE-EMPTY-PERSISTENT-QUEUE"))

;;;
;;;    PERSISTENT-LINKED-QUEUE
;;;
(defclass persistent-linked-queue (persistent-queue)
  ((front :initform '())
   (rear :initform '())
   (count :initform 0 :type integer)))

(defmethod make-empty-persistent-queue ((q persistent-linked-queue))
  (make-instance 'persistent-linked-queue :type (type q)))

(defmethod size ((q persistent-linked-queue))
  (with-slots (count) q
    count))

;;;
;;;    This has to be visible for DEQUE. Likewise INITIALIZE-LIST-QUEUE
;;;    
(defun initialize-linked-queue (q front rear count)
  (let ((new-queue (make-empty-persistent-queue q)))
    (with-slots ((new-front front) (new-rear rear) (new-count count)) new-queue
      (setf new-front front
            new-rear rear
            new-count count))
    new-queue))

(defmethod enqueue ((q persistent-linked-queue) obj)
  (with-slots (front rear count) q
    (if (emptyp q)
        (initialize-linked-queue q (cl:list obj) '() 1)
        (initialize-linked-queue q front (cons obj rear) (1+ count)))) )

(defmethod dequeue ((q persistent-linked-queue))
  (with-slots (front rear count) q
    (if (null (rest front))
        (values (initialize-linked-queue q (cl:reverse rear) '() (1- count)) (front q))
        (values (initialize-linked-queue q (rest front) rear (1- count)) (front q)))) )

(defmethod front ((q persistent-linked-queue))
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
