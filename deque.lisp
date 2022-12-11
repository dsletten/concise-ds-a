;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               deque.lisp
;;;;
;;;;   Started:            Fri Nov  4 02:45:54 2022
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
;;;    ARRAY-RING-BUFFER-DEQUE
;;;    
;(defclass array-deque (deque array-queue) ())
(defclass array-ring-buffer-deque (deque array-ring-buffer) ())

(defun make-array-ring-buffer-deque (&key (type t))
  (make-instance 'array-ring-buffer-deque :type type))

(defmethod enqueue* :before ((dq array-ring-buffer-deque) obj)
  (declare (ignore obj))
  (with-slots (store count) dq
    (when (= count (length store))
      (resize dq))))
(defmethod enqueue* ((dq array-ring-buffer-deque) obj)
  (with-slots (store front count) dq
    (setf front (offset dq -1)
          (aref store (offset dq 0)) obj)
    (incf count)))

(defmethod dequeue* ((dq array-ring-buffer-deque))
  (with-slots (store front count) dq
    (prog1 (rear dq)
      (setf (aref store (offset dq (1- count))) nil)
      (decf count))))

(defmethod rear ((dq array-ring-buffer-deque))
  (with-slots (store count) dq
    (aref store (offset dq (1- count)))) )

;;;
;;;    ARRAY-LIST-DEQUE
;;;    
(defclass array-list-deque (deque array-list-queue) ())

(defun make-array-list-deque (&key (type t))
  (make-instance 'array-list-deque :type type))

(defmethod enqueue* ((dq array-list-deque) obj) ; Likely horrible performance!!!
  (with-slots (list) dq
    (insert list 0 obj)))

(defmethod dequeue* ((dq array-list-deque))
  (with-slots (list) dq
    (delete list -1)))

(defmethod rear ((dq array-list-deque))
  (with-slots (list) dq
    (nth list -1)))

;;;
;;;    LINKED-LIST-DEQUE
;;;    
(defclass linked-list-deque (deque linked-list-queue) ())

(defun make-linked-list-deque (&key (type t))
  (make-instance 'linked-list-deque :type type))

(defmethod enqueue* ((dq linked-list-deque) obj)
  (with-slots (list) dq
    (insert list 0 obj)))

(defmethod dequeue* ((dq linked-list-deque)) ; Not great performance!
  (with-slots (list) dq
    (delete list -1)))

(defmethod rear ((dq linked-list-deque))
  (with-slots (list) dq
    (nth list -1)))

;;;
;;;    Doubly-linked-list deque
;;;    
(defclass dll-deque (deque dll-queue) ())

(defun make-dll-deque (&key (type t))
  (make-instance 'dll-deque :type type))

(defmethod enqueue* ((dq dll-deque) obj)
  (with-slots (list) dq
    (insert list 0 obj)))

(defmethod dequeue* ((dq dll-deque))
  (with-slots (list) dq
    (delete list -1)))

(defmethod rear ((dq dll-deque))
  (with-slots (list) dq
    (nth list -1)))

;;;
;;;   HASH-TABLE-DEQUE
;;;   This must be distinct from HASH-TABLE-QUEUE. See below.
;;;
(defclass hash-table-deque (deque)
  ((store :initform (make-hash-table))
   (front :initform 0)
   (rear :initform 0)))

(defun make-hash-table-deque (&key (type t))
  (make-instance 'hash-table-deque :type type))

(defmethod size ((dq hash-table-deque))
  (with-slots (store) dq
    (hash-table-count store)))

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

;;;
;;;    The following class definition, while simple, does not work due to subtle differences
;;;    in the semantics of FRONT and REAR in HASH-TABLE-QUEUE vs. HASH-TABLE-DEQUE.
;;;    -In HASH-TABLE-QUEUE
;;;     -REAR is the index for the next item to enqueue.
;;;     -As long as the queue is not empty, FRONT is the index of the next item to dequeue.
;;;      (When (= front rear) the queue is empty)
;;;
;;;    -But in HASH-TABLE-DEQUE, FRONT and REAR are symmetric. Either can be an index to
;;;     add another item or an index to current front/rear item. Once the first item has
;;;     been added to an empty deque, both FRONT and REAR are still equal and can both
;;;     be used to retrieve the front item which is also the rear item.
;;;
;;;   HASH-TABLE-DEQUE-X
;;;
;; (defclass hash-table-deque-x (deque hash-table-queue) ())

;; (defun make-hash-table-deque-x (&key (type t))
;;   (make-instance 'hash-table-deque-x :type type))

;; (defmethod enqueue* ((dq hash-table-deque-x) obj)
;;   (with-slots (store front) dq                     <---- Subclass shouldn't know so much about superclass?
;;     (setf (gethash front store) obj)
;;     (decf front)))

;; (defmethod dequeue* ((dq hash-table-deque-x))
;;   (with-slots (store rear) dq
;;     (prog1 (rear dq)
;;       (remhash rear store)
;;       (decf rear))))

;; (defmethod rear ((dq hash-table-deque-x))
;;   (with-slots (store rear) dq
;;     (values (gethash (1- rear) store))))


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
    (let ((new-deque (if (null (rest front))
                         (if (null (rest rear))
                             (make-empty-persistent-queue dq)
                             (initialize-linked-queue dq (rest (cl:reverse rear)) (cl:list (rear dq)) (1- count)))
                         (initialize-linked-queue dq (rest front) rear (1- count)))) )
      (values new-deque (front dq)))) )

(defmethod dequeue* ((dq persistent-linked-deque))
  (with-slots (front rear count) dq
    (let ((new-deque (if (null (rest rear))
                         (if (null (rest front))
                             (make-empty-persistent-queue dq)
                             (initialize-linked-queue dq (cl:list (front dq)) (rest (cl:reverse front)) (1- count)))
                         (initialize-linked-queue dq front (rest rear) (1- count)))) )
      (values new-deque (rear dq)))) )

(defmethod rear ((dq persistent-linked-deque))
  (with-slots (rear) dq
    (first rear)))

;;;
;;;    PERSISTENT-LIST-DEQUE
;;;
(defclass persistent-list-deque (persistent-deque persistent-list-queue) ())

(defmethod make-empty-persistent-queue ((dq persistent-list-deque))
  (make-instance 'persistent-list-deque :type (type dq)))

(defmethod enqueue* ((dq persistent-list-deque) obj)
  (with-slots (list) dq
    (initialize-list-queue dq (insert list 0 obj))))

(defmethod dequeue* ((dq persistent-list-deque))
  (with-slots (list) dq
    (values (initialize-list-queue dq (delete list -1)) (rear dq))))

(defmethod rear ((dq persistent-list-deque))
  (with-slots (list) dq
    (nth list -1)))
