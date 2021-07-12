;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               containers.lisp
;;;;
;;;;   Started:            Sat Jan  2 04:46:01 2021
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
;;;;   Limitations of typed containers:
;;;;   Java
;;;;   Stack<Stack<Integer>> (Hard in Lisp?)
;;;;   Lisp
;;;;   (make-instance 'linked-stack :type '(satisfies evenp))
;;;;   (make-instance 'linked-stack :type '(float -1.0 1.0))
;;;;   (make-instance 'linked-stack :type '(satisfies alpha-char-p))
;;;;
(defpackage :containers 
  (:use :common-lisp)
  (:export :container :type :size :emptyp :clear
           :dispenser
           :stack :push :pop :top
           :array-stack :linked-stack :hash-table-stack :persistent-stack
           :queue :enqueue :dequeue :front
           :array-queue :linked-queue :recycling-queue :ring-buffer :hash-table-queue :persistent-queue)
  (:shadow :type :push :pop))

(in-package :containers)

;;;
;;;    CONTAINER
;;;    
(defclass container ()
  ((type :reader type :initarg :type :initform t))
  (:documentation "A container is an entity that holds finitely many other entities."))

(defgeneric size (container)
  (:documentation "Returns the size of the container."))
(defmethod size ((c container))
  (error "container does not implement SIZE"))

(defgeneric emptyp (container)
  (:documentation "Is the container empty?"))
(defmethod emptyp ((c container))
  (error "container does not implement EMPTYP"))

(defgeneric clear (container)
  (:documentation "Remove all elements from the container"))
(defmethod clear ((c container))
  (error "container does not implement CLEAR"))

;;;
;;;    DISPENSER
;;;    
(defclass dispenser (container)
  ()
  (:documentation "A dispenser is a non-traversable container."))

;;;
;;;    STACK
;;;    
(defclass stack (dispenser)
  ()
  (:documentation "A stack is a dispenser that holds a sequence of elements that can be accessed, inserted, or removed at only one end, the top (LIFO)."))

(defgeneric push (stack obj)
  (:documentation "Push an object onto the top of the stack"))
;; (defmethod push :around ((s stack) obj)
;;   (with-slots (type) s
;;     (if (typep obj type)
;;         (call-next-method)
;;         (error "~A is not of type ~A" obj type))))
(defmethod push :around ((s stack) obj)
  (if (typep obj (type s))
      (call-next-method)
      (error "~A is not of type ~A" obj (type s))))

;;;
;;;    POP should ensure that the reference to the object just removed from the stack has been released
;;;    to allow that object to be GC'd as eligible.
;;;    
(defgeneric pop (stack)
  (:documentation "Pop an object from the top of the stack"))
(defmethod pop :around ((s stack))
  (if (emptyp s)
      (error "Stack is empty")
      (call-next-method)))

(defgeneric top (stack)
  (:documentation "Examine object on the top of the stack"))
(defmethod top :around ((s stack))
  (if (emptyp s)
      (error "Stack is empty")
      (call-next-method)))

;;;
;;;    ARRAY-STACK
;;;    
(defclass array-stack (stack)
  ((store)))

(defmethod initialize-instance :after ((s array-stack) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) s
    (setf store (make-array 20 :adjustable t :fill-pointer 0 :element-type (type s)))) )

(defmethod size ((s array-stack))
  (with-slots (store) s
    (length store)))

(defmethod emptyp ((s array-stack))
  (zerop (size s)))

(defmethod clear ((s array-stack))
  (with-slots (store) s
    (setf (fill-pointer store) 0)))

(defmethod push ((s array-stack) obj)
  (with-slots (store) s
    (vector-push-extend obj store)))

(defmethod pop ((s array-stack))
  (with-slots (store) s
    (vector-pop store)))

(defmethod top ((s array-stack))
  (with-slots (store) s
    (aref store (1- (size s)))) )

;;;
;;;    LINKED-STACK
;;;    
(defclass linked-stack (stack)
  ((top :initform '())
   (count :initform 0)))

(defmethod size ((s linked-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s linked-stack))
  (with-slots (top) s
    (null top)))

(defmethod clear ((s linked-stack))
  (with-slots (top count) s
    (setf top '()
          count 0)))

(defmethod push ((s linked-stack) obj)
  (with-slots (top count) s
;    (cl:push obj top)  ; Duh!
    (setf top (cons obj top))
    (incf count)))

(defmethod pop ((s linked-stack))
  (with-slots (top count) s
;    (cl:pop top)))
;    (prog1 (first top)
    (prog1 (top s)
      (setf top (rest top))
      (decf count))))

(defmethod top ((s linked-stack))
  (with-slots (top) s
    (first top)))

;;;
;;;    HASH-TABLE-STACK
;;;
(defclass hash-table-stack (stack)
  ((store :initform (make-hash-table))))

(defmethod size ((s hash-table-stack))
  (with-slots (store) s
    (hash-table-count store)))

(defmethod emptyp ((s hash-table-stack))
  (zerop (size s)))

(defmethod clear ((s hash-table-stack))
  (with-slots (store) s
    (clrhash store)))

(defmethod push ((s hash-table-stack) obj)
  (with-slots (store) s
    (setf (gethash (1+ (size s)) store) obj)))

(defmethod pop ((s hash-table-stack))
  (with-slots (store) s
;    (prog1 (gethash (size s) store)
    (prog1 (top s)
      (remhash (size s) store))))

(defmethod top ((s hash-table-stack))
  (with-slots (store) s
    (values (gethash (size s) store))))

;;;
;;;    PERSISTENT-STACK (Linked stack)
;;;    - This could be a subclass of LINKED-STACK and simply override CLEAR/PUSH/POP
;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-STACK...
;;;
;;;
;;;    (reduce #'(lambda (s elt) (push s elt)) '(2 4 6 8) :initial-value *ps*)
(defclass persistent-stack (stack)
  ((top :initform '() :initarg :top)
   (count :initform 0 :initarg :count)))

(defmethod size ((s persistent-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s persistent-stack))
  (with-slots (top) s
    (null top)))

;; (defmethod clear ((s persistent-stack))
;;   (with-slots (type) s
;;     (make-instance 'persistent-stack :type type)))
(defmethod clear ((s persistent-stack))
  (make-instance 'persistent-stack :type (type s)))

(defmethod push ((s persistent-stack) obj)
  (with-slots (type top count) s
    (make-instance 'persistent-stack :type type :top (cons obj top) :count (1+ count))))

(defmethod pop ((s persistent-stack))
  (with-slots (type top count) s
;    (values (make-instance 'persistent-stack :top (rest top) :count (1- count)) (first top))))
    (values (make-instance 'persistent-stack :type type :top (rest top) :count (1- count)) (top s))))

(defmethod top ((s persistent-stack))
  (with-slots (top) s
    (first top)))
  
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

(defgeneric front (queue)
  (:documentation "Examine object at the front of the queue"))
(defmethod front :around ((q queue))
  (if (emptyp q)
      (error "Queue is empty")
      (call-next-method)))

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

(defun resize (q)
  (with-slots (store front count) q
    (let* ((length (length store))
           (new-store (make-array (* 2 length) :element-type (type q))))
      (dotimes (i count)
        (setf (aref new-store i) (aref store (mod (+ front i) length))))
      (setf store new-store front 0))))

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
  (zerop (size q)))

(defmethod clear ((q linked-queue))
  (with-slots (front rear count) q
    (setf front nil
          rear nil
          count 0)))

(defmethod enqueue ((q linked-queue) obj)
  (with-slots (front rear count) q
    (cond ((null front)
           (assert (null rear))
           (setf rear (setf front (list obj))))
          (t (setf rear (setf (rest rear) (list obj)))) )
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
    (let ((node (list obj)))
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
        (make-instance 'persistent-queue :type type :front (list obj) :count 1)
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
