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
           :stack :push :pop :peek
           :array-stack :linked-stack :hash-table-stack :persistent-stack
           :queue :enqueue :dequeue :front
           :array-queue :linked-queue :recycling-queue :ring-buffer :hash-table-queue :persistent-queue
           :collection :contains :equals :each
           :iterator :current :next :done
           :list :fill-elt :add :insert :delete :nth :index :slice
           :array-list :singly-linked-list :doubly-linked-list :hash-table-list :persistent-list
           :deque :dll-deque :hash-table-deque :enqueue* :dequeue* :rear
           :list-iterator :remove :current-index :has-next :has-previous)
  (:shadow :type :push :pop :list :nth :delete :remove))

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
  (error "container does not implement SIZE"))

(defgeneric emptyp (container)
  (:documentation "Is the container empty?"))
(defmethod emptyp ((c container))
  (declare (ignore c))
  (error "container does not implement EMPTYP"))

(defgeneric clear (container)
  (:documentation "Remove all elements from the container"))
(defmethod clear ((c container))
  (declare (ignore c))
  (error "container does not implement CLEAR"))

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
  (error "collection does not implement ITERATOR"))

(defgeneric contains (collection obj &key test)
  (:documentation "Does the collection contain the given object? Return that object if found."))
(defmethod contains :around ((c collection) obj &key test)
  (declare (ignore test))
  (if (typep obj (type c))
      (call-next-method)
      (error "~A is not of type ~A" obj (type c))))
(defmethod contains ((c collection) obj &key test)
  (declare (ignore c obj test))
  (error "collection does not implement CONTAINS"))

(defgeneric equals (collection1 collection2 &key test)
  (:documentation "Are the two collections equal?"))
(defmethod equals ((c collection) (d collection) &key test)
  (declare (ignore c d test))
  (error "collection does not implement EQUALS"))

(defgeneric each (collection op)
  (:documentation "Apply operation to each element of collection."))
(defmethod each ((c collection) op)
  (declare (ignore c op))
  (error "collection does not implement EACH"))

;;;
;;;    ITERATOR
;;;
(defclass iterator ()
  ()
  (:documentation "External iterator for a collection."))

;;; FIRST, REWIND???
(defgeneric current (iterator)
  (:documentation "Returns the current element of the iterator traversal."))
(defmethod current :around ((i iterator))
  (if (done i)
      (error "Iteration already finished")
      (call-next-method)))
(defmethod current ((i iterator))
  (declare (ignore i))
  (error "iterator does not implement CURRENT"))

;;; Empty???
(defgeneric next (iterator)
  (:documentation "Advances iterator to the next element of the traversal. Returns that element or NIL if at end."))
(defmethod next ((i iterator))
  (declare (ignore i))
  (error "iterator does not implement NEXT"))

(defgeneric done (iterator)
  (:documentation "Is the traversal completed?"))
(defmethod done ((i iterator))
  (declare (ignore i))
  (error "iterator does not implement DONE"))
