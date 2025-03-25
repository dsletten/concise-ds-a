;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               persistent.lisp
;;;;
;;;;   Started:            Sat Mar 15 21:50:30 2025
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
;;;;   Notes: Extract the persistent types from the larger `Concise' library
;;;;
;;;;

(defpackage :containers 
  (:use :common-lisp)
  (:export :container :type :size :emptyp :clear :elements
           :dispenser
           :collection :contains :equals :each
           :stack :push :pop :peek
           :persistent-stack :persistent-linked-stack
           :queue :enqueue :dequeue :front
           :persistent-queue :persistent-linked-queue
           :deque :enqueue* :dequeue* :rear
           :persistent-deque :persistent-linked-deque
           :iterator :current :next :done
           :list :fill-elt :add :append :insert :delete :nth :index :reverse :slice
           :array-list :singly-linked-list :doubly-linked-list :hash-table-list :persistent-list
           :list-iterator :remove :current-index :has-next :has-previous
           :persistent-list-stack :persistent-list-queue :persistent-list-deque)
  (:shadow :type :emptyp :equals :push :pop :list :nth :delete :remove :fill :reverse :append))

(in-package :containers)

(defun singlep (l)
  (and (consp l) (null (rest l))))

;;;
;;;    CONTAINER
;;;    
(defclass container ()
  ((type :reader type :initarg :type :initform t))
  (:documentation "A container is an entity that holds finitely many other entities."))

(defgeneric size (container)
  (:documentation "Returns the size of the container."))
(defmethod size ((c container))
  (declare (ignore c))
  (error "CONTAINER does not implement SIZE"))

(defgeneric emptyp (container)
  (:documentation "Is the container empty?"))
(defmethod emptyp ((c container))
  (zerop (size c)))

(defgeneric clear (container)
  (:documentation "Remove all elements from the container. No effect if already empty."))
(defmethod clear :around ((c container))
  (unless (emptyp c)
    (call-next-method)))
(defmethod clear ((c container))
  (declare (ignore c))
  (error "CONTAINER does not implement CLEAR"))

(defgeneric fill (container &key count generator)
  (:documentation "Fill up a container for testing purposes."))
(defmethod fill ((c container) &key count generator)
  (declare (ignore c count generator))
  (error "CONTAINER does not implement FILL"))

(defgeneric elements (container)
  (:documentation "Extract the elements of a container as a list."))
(defmethod elements ((c container))
  (declare (ignore c))
  (error "CONTAINER does not implement ELEMENTS"))

;;;
;;;    DISPENSER
;;;    
(defclass dispenser (container)
  ()
  (:documentation "A dispenser is a non-traversable container."))

;;;
;;;    COLLECTION
;;;
(defclass collection (container)
  ()
  (:documentation "A collection is a traversable container."))

(defgeneric iterator (collection)
  (:documentation "Returns an iterator for the collection."))
(defmethod iterator ((c collection))
  (declare (ignore c))
  (error "COLLECTION does not implement ITERATOR"))

(defgeneric contains (collection obj &key test)
  (:documentation "Does the collection contain the given object? Return that object if found."))
(defmethod contains :around ((c collection) obj &key test)
  (declare (ignore test))
  (if (typep obj (type c))
      (call-next-method)
      (error "~S is not of type ~S" obj (type c))))
(defmethod contains ((c collection) obj &key (test #'eql)) ; Check time efficiency
  (do ((iterator (iterator c)))
      ((done iterator) nil)
    (let ((elt (current iterator)))
      (if (funcall test obj elt)
          (return elt)
          (next iterator)))) )

(defgeneric equals (collection1 collection2 &key test)
  (:documentation "Are the two collections equal?"))
(defmethod equals ((c collection) (d collection) &key test)
  (declare (ignore c d test))
  (error "COLLECTION does not implement EQUALS"))

(defgeneric each (collection op)
  (:documentation "Apply operation to each element of collection."))
(defmethod each ((c collection) op)
  (declare (ignore c op))
  (error "COLLECTION does not implement EACH"))

;;;
;;;    STACK
;;;    
(defclass stack (dispenser)
  ()
  (:documentation "A stack is a dispenser that holds a sequence of elements that can be accessed, inserted, or removed at only one end, the top (LIFO)."))

(defgeneric push (stack obj)
  (:documentation "Push an object onto the top of the stack"))
(defmethod push :around ((s stack) obj)
  (if (typep obj (type s))
      (call-next-method)
      (error "~A is not of type ~A" obj (type s))))
(defmethod push ((s stack) obj)
  (declare (ignore s obj))
  (error "STACK does not implement PUSH"))

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
(defmethod pop ((s stack))
  (declare (ignore s))
  (error "STACK does not implement POP"))

(defgeneric peek (stack)
  (:documentation "Examine object on the top of the stack"))
(defmethod peek :around ((s stack))
  (if (emptyp s)
      (error "Stack is empty")
      (call-next-method)))
(defmethod peek ((s stack))
  (declare (ignore s))
  (error "STACK does not implement PEEK"))

;;;
;;;    PERSISTENT-STACK (Linked stack)
;;;    - This could be a subclass of LINKED-STACK and simply override CLEAR/PUSH/POP
;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-STACK...
;;;      - In particular, can't let COUNT be inconsistent with (length top)
;;;      - But inefficient to always calculate COUNT when PUSHing/POPping
;;;      - Initargs removed from slots
;;;      - Still possible to mangle an instance via WITH-SLOTS
;;;
;;;    (reduce #'(lambda (s elt) (push s elt)) '(2 4 6 8) :initial-value *ps*)
(defclass persistent-stack (stack)
  ()
  (:documentation "A stack that defines non-destructive operations."))

;;;
;;;    This changes signature of CONTAINER?
;;;    
(defmethod clear :around ((s persistent-stack))
  (if (emptyp s)
      s
      (call-next-method)))
(defmethod clear ((s persistent-stack))
  (make-empty-persistent-stack s))

(defmethod fill ((stack persistent-stack) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-stack = (push stack (funcall generator i)) then (push new-stack (funcall generator i))   ; ??????? Scope problem without renaming?!??!
        finally (return new-stack)))

(defmethod elements ((stack persistent-stack))
  (loop for new-stack = stack then (pop new-stack)
        until (emptyp new-stack)
        collect (peek new-stack)))

(defgeneric make-empty-persistent-stack (s)
  (:documentation "Create an empty persistent stack of a given element type."))
(defmethod make-empty-persistent-stack ((s persistent-stack))
  (declare (ignore s))
  (error "PERSISTENT-STACK does not implement MAKE-EMPTY-PERSISTENT-STACK"))

;;;
;;;    PERSISTENT-LINKED-STACK
;;;
(defclass persistent-linked-stack (persistent-stack)
  ((top :initform '())
   (count :initform 0 :type integer)))

(defmethod make-empty-persistent-stack ((s persistent-linked-stack))
  (make-instance 'persistent-linked-stack :type (type s)))

(defmethod size ((s persistent-linked-stack))
  (with-slots (count) s
    count))

(defmethod emptyp ((s persistent-linked-stack))
  (with-slots (top) s
    (null top)))

(flet ((initialize-stack (s top count)
         (let ((new-stack (make-empty-persistent-stack s)))
           (with-slots ((new-top top) (new-count count)) new-stack
             (setf new-top top
                   new-count count))
           new-stack)))
  (defmethod push ((s persistent-linked-stack) obj)
    (with-slots (top count) s
      (initialize-stack s (cons obj top) (1+ count))))
  (defmethod pop ((s persistent-linked-stack))
    (with-slots (top count) s
      (values (initialize-stack s (rest top) (1- count)) (peek s)))) )

(defmethod peek ((s persistent-linked-stack))
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
(defmethod enqueue :around ((q queue) obj)
  (if (typep obj (type q))
      (call-next-method)
      (error "~A is not of type ~A" obj (type q))))
(defmethod enqueue ((q queue) obj)
  (declare (ignore q obj))
  (error "QUEUE does not implement ENQUEUE"))

;;;
;;;    DEQUEUE should ensure that reference to object being removed from queue is actually removed in order to
;;;    allow the object to be eligible for GC.
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

(defmethod elements ((queue queue))
  (loop until (emptyp queue)
        collect (dequeue queue)))

;;;
;;;    PERSISTENT-QUEUE (Linked queue)
;;;    - Invariant: Whenever queue is not empty, front list must be non-empty.
;;;    - Don't want client to be able to MAKE-INSTANCE of non-empty PERSISTENT-QUEUE...
;;;      - Similar issues to PERSISTENT-STACK
;;;      
(defclass persistent-queue (queue)
  ()
  (:documentation "A queue that defines non-destructive operations."))

(defgeneric make-empty-persistent-queue (q)
  (:documentation "Create an empty persistent queue of a given element type."))
(defmethod make-empty-persistent-queue ((q persistent-queue))
  (declare (ignore q))
  (error "PERSISTENT-QUEUE does not implement MAKE-EMPTY-PERSISTENT-QUEUE"))

;;;
;;;    This changes signature of CONTAINER?
;;;    
(defmethod clear :around ((q persistent-queue))
  (if (emptyp q)
      q
      (call-next-method)))
(defmethod clear ((q persistent-queue))
  (make-empty-persistent-queue q))

(defmethod fill ((queue persistent-queue) &key (count 1000) (generator #'identity))
  (loop for i from 1 to count
        for new-queue = (enqueue queue (funcall generator i)) then (enqueue new-queue (funcall generator i))
        finally (return new-queue)))

(defmethod elements ((queue persistent-queue))
  (loop for new-queue = queue then (dequeue new-queue)
        until (emptyp new-queue)
        collect (front new-queue)))

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
        (initialize-linked-queue q (cons obj nil) '() 1)
        (initialize-linked-queue q front (cons obj rear) (1+ count)))) )

(defmethod dequeue ((q persistent-linked-queue))
  (with-slots (front rear count) q
    (if (singlep front)
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

;;;
;;;    DEQUE (Double-ended queue)
;;;
(defclass deque (queue) ; Inherited error msgs???
  ()
  (:documentation "A deque is a double-ended queue allowing ENQUEUE and DEQUEUE operations on both the front and rear."))

(defgeneric enqueue* (deque obj)
  (:documentation "Enqueue an object at the front of the deque"))
(defmethod enqueue* :around ((dq deque) obj)
  (if (typep obj (type dq))
      (call-next-method)
      (error "~A is not of type ~A" obj (type dq))))
(defmethod enqueue* ((dq deque) obj)
  (declare (ignore dq obj))
  (error "DEQUE does not implement ENQUEUE*"))

(defgeneric dequeue* (deque)
  (:documentation "Remove an object from the rear of the deque"))
(defmethod dequeue* :around ((dq deque))
  (if (emptyp dq)
      (error "Deque is empty")
      (call-next-method)))
(defmethod dequeue* ((dq deque))
  (declare (ignore dq))
  (error "DEQUE does not implement DEQUEUE*"))

(defgeneric rear (deque)
  (:documentation "Examine object at the rear of the deque"))
(defmethod rear :around ((dq deque))
  (if (emptyp dq)
      (error "Deque is empty")
      (call-next-method)))
(defmethod rear ((dq deque))
  (declare (ignore dq))
  (error "DEQUE does not implement REAR"))

;;;
;;;    PERSISTENT-DEQUE
;;;
(defclass persistent-deque (persistent-queue deque)
  ()
  (:documentation "A deque that defines non-destructive operations."))

;;;
;;;    PERSISTENT-LINKED-DEQUE
;;;    - Invariant: Tail of FRONT and head of REAR share same first enqueued elt.
;;;                 FRONT and REAR only null when deque is empty.
;;;    
(defclass persistent-linked-deque (persistent-deque persistent-linked-queue) ())

(defmethod make-empty-persistent-queue ((dq persistent-linked-deque))
  (make-instance 'persistent-linked-deque :type (type dq)))

(defmethod enqueue ((dq persistent-linked-deque) obj)
  (with-slots (front rear count) dq
    (if (emptyp dq)
        (initialize-linked-queue dq (cl:list obj) (cl:list obj) 1)
        (initialize-linked-queue dq front (cons obj rear) (1+ count)))) )

(defmethod enqueue* ((dq persistent-linked-deque) obj)
  (with-slots (front rear count) dq
    (if (emptyp dq)
        (initialize-linked-queue dq (cl:list obj) (cl:list obj) 1)
        (initialize-linked-queue dq (cons obj front) rear (1+ count)))) )

(defmethod dequeue ((dq persistent-linked-deque))
  (with-slots (front rear count) dq
    (let ((new-deque (if (singlep front)
                         (if (singlep rear)
                             (make-empty-persistent-queue dq)
                             (initialize-linked-queue dq (rest (cl:reverse rear)) (cl:list (rear dq)) (1- count)))
                         (initialize-linked-queue dq (rest front) rear (1- count)))) )
      (values new-deque (front dq)))) )

(defmethod dequeue* ((dq persistent-linked-deque))
  (with-slots (front rear count) dq
    (let ((new-deque (if (singlep rear)
                         (if (singlep front)
                             (make-empty-persistent-queue dq)
                             (initialize-linked-queue dq (cl:list (front dq)) (rest (cl:reverse front)) (1- count)))
                         (initialize-linked-queue dq front (rest rear) (1- count)))) )
      (values new-deque (rear dq)))) )

(defmethod rear ((dq persistent-linked-deque))
  (with-slots (rear) dq
    (first rear)))

