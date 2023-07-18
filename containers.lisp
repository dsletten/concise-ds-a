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
;;;;   
;;;;   Lisp
;;;;   (make-instance 'linked-stack :type '(satisfies evenp))
;;;;   (make-instance 'linked-stack :type '(float -1.0 1.0))
;;;;   (make-instance 'linked-stack :type '(satisfies alpha-char-p))
;;;;
;;;;   TODO:
;;;;    - Other methods: ELEMENTS
;;;;    - DISCHARGE method - CLEAR + return elts
(load "/home/slytobias/lisp/packages/lang.lisp")

(defpackage :containers 
  (:use :common-lisp :lang)
  (:export :container :type :size :emptyp :clear :elements
           :dispenser
           :stack :push :pop :peek
           :array-stack :linked-stack :hash-table-stack :array-list-stack :linked-list-stack
           :persistent-stack :persistent-linked-stack :persistent-list-stack
           :queue :enqueue :dequeue :front
           :ring-buffer :array-ring-buffer :linked-ring-buffer
           :linked-queue :circular-queue :recycling-queue :hash-table-queue
           :array-list-queue :linked-list-queue :dll-queue
           :persistent-queue :persistent-linked-queue :persistent-list-queue
           :collection :contains :equals :each
           :iterator :current :next :done
           :list :fill-elt :add :append :insert :delete :nth :index :reverse :slice
           :array-list :singly-linked-list :doubly-linked-list :hash-table-list :persistent-list
           :deque :dll-deque :hash-table-deque :enqueue* :dequeue* :rear
           :list-iterator :remove :current-index :has-next :has-previous)
  (:shadow :type :emptyp :push :pop :list :nth :delete :remove :fill :reverse :append))

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
  (declare (ignore c))
  (error "CONTAINER does not implement SIZE"))

(defgeneric emptyp (container)
  (:documentation "Is the container empty?"))
(defmethod emptyp ((c container))
  (declare (ignore c))
  (error "CONTAINER does not implement EMPTYP"))

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
;; (defmethod contains ((c collection) obj &key test)
;;   (declare (ignore c obj test))
;;   (error "COLLECTION does not implement CONTAINS"))

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

;; (defgeneric fill (collection &optional count)
;;   (:documentation "Fill a collection with COUNT values for testing."))
;; (defmethod fill ((c collection) &optional count)
;;   (declare (ignore c count))
;;   (error "COLLECTION does not implement FILL"))

;;;
;;;    MUTABLE-COLLECTION
;;;    
(defclass mutable-collection (collection)
  ((modification-count :initform 0))
  (:documentation "A collection whose structure and elements can be modified."))

(defgeneric count-modification (collection)
  (:documentation "Increase the MODIFICATION-COUNT of this collection."))
(defmethod count-modification ((c mutable-collection))
  (with-slots (modification-count) c
    (incf modification-count)))

;;;
;;;    CURSOR
;;;
(defclass cursor ()
  ((done :initarg :done :documentation "A function that determines whether traversal is complete.")
   (current :initarg :current :documentation "A function that yields the current element of the collection.")
   (advance :initarg :advance :documentation "A function that moves to the next element if any.")))

;;;
;;;    ITERATOR
;;;
;;;    Design conflict
;;;    I.  Put fewer details on abstract ITERATOR allowing wider variety of subclasses.
;;;        For example, PERSISTENT-LIST-ITERATOR can reasonably be a subclass if no
;;;        EXPECTED-MODIFICATION-COUNT, no :AROUND methods
;;;    II. Eliminate duplication by moving EXPECTED-MODIFICATION-COUNT, :AROUND methods up
;;;        from subclasses. PERSISTENT-LIST-ITERATOR simply has EXPECTED-MODIFICATION-COUNT of 0???
;;;        Kind of gross??
;;;
;;;
;;;
;;;    Use PARENTS stack for tree iterator (like HISTORY for SINGLY-LINKED-LIST LIST-ITERATOR)
;;;    - Binary tree: push parent
;;;    - Arbitrary tree: push parent + remaining children
;;;    
(defclass iterator ()
  ((cursor :initarg :cursor))
  (:documentation "External iterator for a collection."))

(defgeneric done (iterator)
  (:documentation "Is the traversal completed?"))
(defmethod done ((i iterator))
  (with-slots (cursor) i
    (with-slots (done) cursor
      (funcall done))))

;;; FIRST, REWIND???
(defgeneric current (iterator)
  (:documentation "Returns the current element of the iterator traversal."))
(defmethod current :around ((i iterator))
  (if (done i)
      (error "Iteration already finished")
      (call-next-method)))
(defmethod current ((i iterator))
  (with-slots (cursor) i
    (with-slots (current) cursor
      (funcall current))))

;;; Empty???
(defgeneric next (iterator)
  (:documentation "Advances iterator to the next element of the traversal. Returns that element or NIL if at end."))
(defmethod next ((i iterator))
  (with-slots (cursor) i
    (cond ((done i) nil)
          (t (with-slots (advance) cursor
               (funcall advance))
             (if (done i)
                 nil
                 (current i)))) ))

;;;
;;;    MUTABLE-COLLECTION-ITERATOR
;;;    
(defclass mutable-collection-iterator (iterator)
  ((modification-count :initarg :modification-count :documentation "A function that produces the modification count for the collection.")
   (expected-modification-count :type integer))
  (:documentation "External iterator for a mutable collection."))

(defmethod initialize-instance :after ((i mutable-collection-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (modification-count expected-modification-count) i
    (setf expected-modification-count (funcall modification-count))))

(defgeneric co-modified (iterator)
  (:documentation "Check whether structure of underlying collection has been changed."))
(defmethod co-modified ((i mutable-collection-iterator))
  (with-slots (modification-count expected-modification-count) i
    (/= expected-modification-count (funcall modification-count))))

(defgeneric check-co-modification (iterator)
  (:documentation "Signal error when collection structure has changed."))
(defmethod check-co-modification ((i mutable-collection-iterator))
  (when (co-modified i)
    (error "Iterator invalid due to structural modification of collection")))

(defmethod done :around ((i mutable-collection-iterator))
  (check-co-modification i)
  (call-next-method))
    
(defmethod current :around ((i mutable-collection-iterator))
  (check-co-modification i) ; Already checked by DONE!!
  (call-next-method))

(defmethod next :around ((i mutable-collection-iterator))
  (check-co-modification i)
  (call-next-method))

;;;
;;;    PERSISTENT-COLLECTION-ITERATOR
;;;    
(defclass persistent-collection-iterator (iterator) ())

(defmethod next ((i persistent-collection-iterator))
  (with-slots (cursor) i
    (cond ((done i) i)
          (t (with-slots (advance) cursor
               (let ((new-iterator (funcall advance)))
                 (if (done new-iterator)
                     (values new-iterator nil)
                     (values new-iterator (current new-iterator)))) )))) )
