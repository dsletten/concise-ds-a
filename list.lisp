;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               list.lisp
;;;;
;;;;   Started:            Sat Nov 13 14:14:17 2021
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
;;;;   Notes: Problems as usual with persistent version...
;;;;   - Java returns different type for INSERT/DELETE
;;;;   - :AROUND method in Lisp short-circuits out-of-bounds index for DELETE
;;;;
;;;;   Interesting symmetry in :AROUND methods: INSERT == SETF NTH, DELETE == NTH
;;;;
;;;;   No conflict between NEXT/PREVIOUS methods of DCONS and iterators???
;;;;
;;;;
;;;;   Should iterator/list-iterator be slots of list objects? Calling ITERATOR/LIST-ITERATOR would
;;;;   return a copy that could be used independently? This would allow the list to keep track of its
;;;;   iterators and invalidate/update them when the list itself is modified...
;;;;   - List keeps map of iterators created prior to modification. All are marked invalid?
;;;;     (This would screw up GC!)
;;;;
;;;;   Iterator becomes invalidated if associated list is structurally modified?
;;;;   List iterator is still valid after its ADD/REMOVE methods are called... How about list methods?
;;;;     

(in-package :containers)

;; (defvar *sll* (make-instance 'singly-linked-list :type '(satisfies evenp) :fill-elt 0))
;; (add *sll* 2 4 6)
;; *sll* => #<SINGLY-LINKED-LIST (2 4 6)>
;; (add *sll* 3)
;; *** - Type mismatch with OBJS

;;;
;;;    LIST
;;;
(defclass list (collection)
  ((fill-elt :reader fill-elt :initform nil :initarg :fill-elt)) ; Set type based on FILL-ELT (Compound type??)
  (:documentation "An ordered linear collection."))

(defmethod initialize-instance :after ((l list) &rest initargs &key fill-elt)
  (declare (ignore initargs))           
  (assert (typep fill-elt (type l)) () "Incompatible FILL-ELT type: ~D should be: ~D" (type-of fill-elt) (type l)))

(defmethod print-object ((l list) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "(")
    (loop with i = (iterator l)
          until (done i)
          do (format stream "~A" (current i))
             (next i)
             (unless (done i)
               (format stream " ")))
    (format stream ")")))

(defmethod equals ((l1 list) (l2 list) &key (test #'eql))
  (if (= (size l1) (size l2))
      (do ((i1 (iterator l1))
           (i2 (iterator l2)))
          ((and (done i1) (done i2)) t)
        (unless (funcall test (current i1) (current i2))
          (return nil))
        (next i1)
        (next i2))
      nil))

(defmethod each ((l list) op)
  (let ((i (iterator l)))
    (loop until (done i)
          do (funcall op (current i))
             (next i))))

(defgeneric list-iterator (list &optional start)
  (:documentation "Returns a list iterator for the list."))
(defmethod list-iterator ((l list) &optional start)
  (declare (ignore l start))
  (error "list does not implement LIST-ITERATOR"))

;;;
;;;    See semantics of https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/Collection.html#add(E)
;;;    Regarding return value.
;;;    - Should be part of COLLECTION interface?
;;;    
;; (defgeneric add (list obj)
;;   (:documentation "Add the object at the end of the list."))
;; (defmethod add :around ((l list) obj)
;;   (if (or (null obj) (typep obj (type l)))
;;       (call-next-method)
;;       (error "~A is not of type ~A" obj (type l))))
;; (defmethod add ((l list) obj)
;;   (declare (ignore l obj))
;;   (error "list does not implement ADD"))

(defgeneric add (list &rest objs)
  (:documentation "Add the objects to the end of the list."))
(defmethod add :around ((l list) &rest objs)
  (if (every #'(lambda (obj) (typep obj (type l))) objs)
      (call-next-method)
      (error "Type mismatch with OBJS")))
(defmethod add ((l list) &rest objs)
  (declare (ignore l objs))
  (error "list does not implement ADD"))

(defun extend-list (list i obj)
  (apply #'add list (loop repeat (1+ (- i (size list)))
                          for tail = (cl:list obj) then (cons (fill-elt list) tail)
                          finally (return tail))))

;;;
;;;    INSERT multiples like ADD?
;;;    Negative index only makes sense for non-empty list.
;;;    
(defgeneric insert (list i obj)
  (:documentation "Insert the object at the given index. List is extended as necessary."))
(defmethod insert :around ((l list) (i integer) obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((minusp i)
         (let ((j (+ i (size l)))) ; {-n, ..., -1} -> {0, ..., n-1}
           (unless (minusp j)
             (insert l j obj))))
        ((>= i (size l)) (extend-list l i obj)) ; Also if list is empty.
        (t (call-next-method))))
(defmethod insert ((l list) (i integer) obj)
  (declare (ignore l i obj))
  (error "list does not implement INSERT"))

(defgeneric delete (list i &key &allow-other-keys)
  (:documentation "Delete the object at the given index."))
(defmethod delete :around ((l list) (i integer) &key &allow-other-keys)
  (cond ((emptyp l) (error "List is empty"))
        ((minusp i)
         (let ((j (+ i (size l)))) ; {-n, ..., -1} -> {0, ..., n-1}
           (unless (minusp j)
             (delete l j))))
        ((< i (size l)) (call-next-method))))
(defmethod delete :around ((l list) node &key &allow-other-keys)
  (cond ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod delete ((l list) (i integer) &key &allow-other-keys)
  (declare (ignore l i))
  (error "list does not implement DELETE"))
(defmethod delete ((l list) node &key &allow-other-keys)
  (declare (ignore l node))
  (error "list does not implement DELETE"))

;;;
;;;    Should return FILL-ELT rather than NIL?
;;;    
(defgeneric nth (list i)
  (:documentation "Retrieve the object at the given index."))
(defmethod nth :around ((l list) (i integer))
  (cond ((minusp i)
         (let ((j (+ i (size l)))) ; {-n, ..., -1} -> {0, ..., n-1}
           (if (minusp j)
               nil
               (nth l j))))
        ((>= i (size l)) nil)
        (t (call-next-method))))
(defmethod nth ((l list) (i integer))
  (declare (ignore l i))
  (error "list does not implement NTH"))

(defgeneric (setf nth) (obj list i)
  (:documentation "Assign the object at the given index."))
(defmethod (setf nth) :around (obj (l list) (i integer))
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((minusp i)
         (let ((j (+ i (size l)))) ; {-n, ..., -1} -> {0, ..., n-1}
           (unless (minusp j)
             (setf (nth l j) obj))))
        ((>= i (size l)) (extend-list l i obj))
        (t (call-next-method))))
(defmethod (setf nth) (obj (l list) (i integer))
  (declare (ignore l i obj))
  (error "list does not implement (SETF NTH)"))

(defgeneric index (list obj &key test)
  (:documentation "Determine index of the object if present in the list."))
(defmethod index :around ((l list) obj &key test)
  (declare (ignore test)) ; Why is this needed?!
  (if (typep obj (type l))
      (call-next-method)
      (error "~A is not of type ~A" obj (type l))))
(defmethod index ((l list) obj &key test)
  (declare (ignore l obj test))
  (error "list does not implement INDEX"))

(defgeneric slice (list i n)
  (:documentation "Return the n-element sublist of the list starting at index i. The index may be negative, however, if the index points beyond the beginning of the list an empty sublist is returned."))
(defmethod slice :around ((l list) (i integer) (n integer))
  (cond ((< n 0) (error "Count N must be non-negative: ~D" n))
        ((minusp i)
         (let ((j (+ i (size l))))
           (if (minusp j)
               (slice l 0 0)
               (slice l j n))))
        (t (call-next-method))))
(defmethod slice ((l list) (i integer) (n integer))
  (declare (ignore l i n))
  (error "list does not implement SLICE"))

(defclass linked-list (list)
  ()
  (:documentation "A list implemented with linked nodes."))

(defgeneric insert-before (list node obj)
  (:documentation "Insert the object before the specified node."))
(defmethod insert-before :around ((l linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-before ((l linked-list) node obj)
  (declare (ignore l node obj))
  (error "linked-list does not implement INSERT"))

(defgeneric insert-after (list node obj)
  (:documentation "Insert the object after the specified node."))
(defmethod insert-after :around ((l linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-after ((l linked-list) node obj)
  (declare (ignore l node obj))
  (error "linked-list does not implement INSERT"))

;;;
;;;    ARRAY-LIST
;;;    
(defclass array-list (list)
  ((store)
   (modification-count :initform 0)))

(defmethod initialize-instance :after ((l array-list) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) l
    (setf store (make-array 20 :adjustable t :fill-pointer 0 :element-type (type l)))) )

(defun make-array-list (&key (type t) (fill-elt nil))
 (make-instance 'array-list :type type :fill-elt fill-elt))

(defmethod size ((l array-list))
  (with-slots (store) l
    (length store)))

(defmethod emptyp ((l array-list))
  (zerop (size l)))

(defmethod clear ((l array-list))
  (with-slots (store modification-count) l
    (incf modification-count)
    (setf (fill-pointer store) 0)))

(defmethod iterator ((l array-list))
  (make-instance 'random-access-list-iterator :list l))

(defmethod list-iterator ((l array-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l array-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

(defmethod add ((l array-list) &rest objs)
  (unless (null objs)
    (with-slots (store modification-count) l
      (incf modification-count) ; Only 1?
      (dolist (obj objs)
        (vector-push-extend obj store)))) )

;;;
;;;    i < -size => error or no effect?
;;;    Add elt to end: (insert al (size al) x)
;;;    Can't do this with negative index
;;;    
(defmethod insert ((l array-list) (i integer) obj)
  (with-slots (store modification-count) l
    (incf modification-count)
    (vector-push-extend (fill-elt l) store)
    (setf (subseq store (1+ i)) (subseq store i)
          (aref store i) obj)))

(defmethod delete ((l array-list) (i integer) &key &allow-other-keys)
  (with-slots (store modification-count) l
    (incf modification-count)
    (prog1 (aref store i)
      (setf (subseq store i) (subseq store (1+ i)))
      (vector-pop store))))

(defmethod nth ((l array-list) (i integer))
  (with-slots (store) l
    (aref store i)))

;;;
;;;    D'oh!
;;;    (setf (nth *al* 11) (nth *al* 11)) => Error: NIL is not of type INTEGER
;;;
;;;    (setf (nth *al* 12 :fill-elt 0) 44)  !!
(defmethod (setf nth) (obj (l array-list) (i integer))
  (with-slots (store) l
    (setf (aref store i) obj)))

(defmethod index ((l array-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

(defmethod slice ((l array-list) (i integer) (n integer))
  (with-slots (store) l
    (let ((al (make-array-list :type (type l) :fill-elt (fill-elt l)))
          (count (size l)))
      (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (aref store k)))
      al)))

;;;
;;;    Used by ARRAY-LIST and HASH-TABLE-LIST
;;;    
(defclass random-access-list-iterator (iterator)
  ((list :initarg :list)
   (cursor :initform 0 :type integer)
   (expected-modification-count :type integer)))

(defmethod initialize-instance :after ((i random-access-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (list expected-modification-count) i
    (setf expected-modification-count (slot-value list 'modification-count))))

(defun co-modified (iterator)
  (with-slots (list expected-modification-count) iterator
    (/= expected-modification-count (slot-value list 'modification-count))))

(defmethod current :around ((i random-access-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current ((i random-access-list-iterator))
  (with-slots (list cursor) i
    (nth list cursor)))

(defmethod next :around ((i random-access-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod next ((i random-access-list-iterator))
  (with-slots (cursor) i
    (cond ((done i) nil)
          (t (incf cursor)
             (if (done i)
                 nil
                 (current i)))) ))

(defmethod done :around ((i random-access-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod done ((i random-access-list-iterator))
  (with-slots (list cursor) i
    (assert (<= cursor (size list)) () "Index is out of bounds: ~D" cursor)
    (= cursor (size list))))


;;;
;;;    SINGLY-LINKED-LIST
;;;    
(defclass singly-linked-list (linked-list)
  ((store :initform '())
   (count :initform 0)
   (modification-count :initform 0)))

(defun make-linked-list (&key (type t) (fill-elt nil))
  (make-instance 'singly-linked-list :type type :fill-elt fill-elt))

(defmethod size ((l singly-linked-list))
  (slot-value l 'count))

(defmethod emptyp ((l singly-linked-list))
  (null (slot-value l 'store)))

(defmethod clear ((l singly-linked-list))
  (with-slots (store count modification-count) l
    (incf modification-count)
    (setf store '()
          count 0)))

(defmethod iterator ((l singly-linked-list))
  (make-instance 'singly-linked-list-iterator :list l))

(defmethod list-iterator ((l singly-linked-list) &optional (start 0))
  (make-instance 'singly-linked-list-list-iterator :list l :start start))

(defmethod contains ((l singly-linked-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

;; (defmethod contains ((l singly-linked-list) obj &key (test #'eql))
;;   (loop with iterator = (iterator l)
;;         until (done iterator)
;;         when (funcall test obj (current iterator))
;;           do (return t)
;;         else 
;;           do (next iterator)
;;         finally (return nil)))

;;;
;;;    Copy OBJS list and find its length in one traversal.
;;;    
(defmethod add ((l singly-linked-list) &rest objs)
  (unless (null objs)
    (with-slots (store count modification-count) l
      (incf modification-count) ; Only 1?
      (loop for i from 0
            for elt in objs
            collect elt into elts
            finally (progn (setf store (nconc store elts))
                           (incf count i)))) ))

(defmethod insert ((l singly-linked-list) (i integer) obj)
  (with-slots (store) l
    (insert-before l (nthcdr i store) obj)))

(defmethod insert-before ((l singly-linked-list) node obj)
  (let ((tail (cons (first node) (rest node))))
    (setf (first node) obj
          (rest node) tail)))

(defmethod insert-before :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count modification-count) l
    (incf modification-count)
    (incf count)))

(defmethod insert-after ((l singly-linked-list) node obj)
  (let ((tail (cons obj (rest node))))
    (setf (rest node) tail)))

(defmethod insert-after :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count modification-count) l
    (incf modification-count)
    (incf count)))

;; (defmethod delete ((l singly-linked-list) (i integer))
;;   (with-slots (store) l
;;     (if (zerop i)
;;         (cl:pop store)
;;         (let ((head (nthcdr (1- i) store)))
;;           (prog1 (first (rest head))
;;             (setf (rest head) (rest (rest head)))) ))))

;; (defmethod delete ((l singly-linked-list) (i integer) &key &allow-other-keys)
;;   (with-slots (store count) l
;;     (if (zerop i)
;;         (prog1 (cl:pop store) (decf count)) ; Ugh!!
;;         (delete l (nthcdr (1- i) store)))) )

(defmethod delete ((l singly-linked-list) (i integer) &key &allow-other-keys)
  (with-slots (store count) l
    (if (zerop i)
        (delete l store)
        (delete l (nthcdr (1- i) store) :parent t))) )

;;;    Doesn't need :AROUND method? REMOVE has :AROUND method...
;;;    No way to confirm that NODE is actually part of list? Doesn't matter here? See DLL
;;;
;;;    HEAD is CONS whose CDR is the node to delete. Cannot remove first CONS of list here.
;;;    Must be at least 2-element list...
;;;    1. Can't DELETE from empty list
;;;    2. Single-elt list must be treated specially.
;;;    (The approach below can't remove first CONS. Single-elt list is just special case of that.)
;;;    
;;;    HEAD -> NODE -> NEXT (Poss. NIL)
;;;    
;; (defmethod delete ((l singly-linked-list) (head cons) &key (parent nil))
;;   (let ((node (rest head)))
;; ;    (assert (not (null node)) () ":AROUND method should prevent this...")
;;     (prog1 (first node)
;;       (setf (rest head) (rest node)))) )
;;       ;; (let ((next (rest node)))
;;       ;;   (if (null next)
;;       ;;       (setf (rest head) nil)
;;       ;;       (setf (first node) (first next)
;;       ;;             (rest node) (rest next)))) )))

;;;
;;;    :PARENT - Handle all cases but first node
;;;    :CHILD - Handle first node (possibly only node)
;;;    
(defmethod delete ((l singly-linked-list) (node cons) &key (parent nil))
;    (assert (not (null node)) () ":AROUND method should prevent this...")  ??????
  (with-slots (store) l
    (let ((child (rest node)))
      (if parent
          (prog1 (first child)
            (setf (rest node) (rest child)))
          (prog1 (first node)
            (if (null child)
                (setf store '())
                (setf (first node) (first child)
                      (rest node) (rest child)))) ))))

;(defmethod delete :after ((l singly-linked-list) (i integer))
(defmethod delete :after ((l singly-linked-list) (node cons) &key &allow-other-keys)
  (declare (ignore node))
  (with-slots (count modification-count) l
    (incf modification-count)
    (decf count)))

(defmethod nth ((l singly-linked-list) (i integer))
  (with-slots (store count) l
    (cl:nth i store)))

(defmethod (setf nth) (obj (l singly-linked-list) (i integer))
  (with-slots (store) l
    (setf (cl:nth i store) obj)))

(defmethod index ((l singly-linked-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

(defmethod slice ((l singly-linked-list) (i integer) (n integer))
  (with-slots (store count) l
    (let ((sll (make-linked-list :type (type l) :fill-elt (fill-elt l))))
      (apply #'add sll (subseq store (min i count) (min (+ i n) count)))
      sll)))

;;;
;;;    Don't need to hold onto LIST after initialization?
;;;    CURSOR added to avoid manipulating LIST directly...
;;;    
(defclass singly-linked-list-iterator (iterator)
  ((list :initarg :list)
   (cursor)
   (expected-modification-count :type integer)))

(defmethod initialize-instance :after ((i singly-linked-list-iterator) &rest initargs &key list)
  (declare (ignore initargs))
  (with-slots (cursor expected-modification-count) i
    (setf cursor (slot-value list 'store) ; Inappropriate access?
          expected-modification-count (slot-value list 'modification-count))))

(defmethod current :around ((i singly-linked-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current ((i singly-linked-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod next :around ((i singly-linked-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod next ((i singly-linked-list-iterator))
  (with-slots (cursor) i
    (cond ((done i) nil)
          (t (cl:pop cursor)
             (if (done i)
                 nil
                 (current i)))) ))

(defmethod done :around ((i singly-linked-list-iterator))
  (if (co-modified i)
      (error "Iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod done ((i singly-linked-list-iterator))
  (with-slots (cursor) i
    (null cursor)))


;;;
;;;    Certain operations are marginally faster with doubly-linked-list due to cursor.
;;;    
;; ? (time (let ((list (make-instance 'doubly-linked-list))) (fill-list list 1000) (dotimes (i 1000) (nth list i))))
;; (LET ((LIST (MAKE-INSTANCE 'DOUBLY-LINKED-LIST))) (FILL-LIST LIST 1000) (DOTIMES (I 1000) (NTH LIST I)))
;; took 2,775 microseconds (0.002775 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      1,076 microseconds (0.001076 seconds) were spent in user mode
;;      1,702 microseconds (0.001702 seconds) were spent in system mode
;;  144,192 bytes of memory allocated.
;; NIL
;; ? (time (let ((list (make-instance 'singly-linked-list))) (fill-list list 1000) (dotimes (i 1000) (nth list i))))
;; (LET ((LIST (MAKE-INSTANCE 'SINGLY-LINKED-LIST))) (FILL-LIST LIST 1000) (DOTIMES (I 1000) (NTH LIST I)))
;; took 3,393 microseconds (0.003393 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      3,393 microseconds (0.003393 seconds) were spent in user mode
;;          0 microseconds (0.000000 seconds) were spent in system mode
;;  64,080 bytes of memory allocated.
;; NIL

;;;
;;;    But results vary!!
;;;
;; ? (time (let ((list (make-instance 'singly-linked-list))) (fill-list list 1000) (dotimes (i 1000) (nth list (random 1000)))))
;; (LET ((LIST (MAKE-INSTANCE 'SINGLY-LINKED-LIST))) (FILL-LIST LIST 1000) (DOTIMES (I 1000) (NTH LIST (RANDOM 1000))))
;; took 3,676 microseconds (0.003676 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      1,223 microseconds (0.001223 seconds) were spent in user mode
;;      2,456 microseconds (0.002456 seconds) were spent in system mode
;;  64,080 bytes of memory allocated.
;; NIL
;; ? (time (let ((list (make-instance 'doubly-linked-list))) (fill-list list 1000) (dotimes (i 1000) (nth list (random 1000)))))
;; (LET ((LIST (MAKE-INSTANCE 'DOUBLY-LINKED-LIST))) (FILL-LIST LIST 1000) (DOTIMES (I 1000) (NTH LIST (RANDOM 1000))))
;; took 28,400 microseconds (0.028400 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      28,399 microseconds (0.028399 seconds) were spent in user mode
;;           0 microseconds (0.000000 seconds) were spent in system mode
;;  136,672 bytes of memory allocated.
;; NIL

;;;
;;;    DOUBLY-LINKED-LIST
;;;    - Circular
;;;    - Cursor
;;;    
(defclass dcons ()
  ((content :accessor content :initarg :content :initform nil)
   (previous :accessor previous :initarg :previous :initform nil :type (or null dcons))
   (next :accessor next :initarg :next :initform nil :type (or null dcons))))

;;;
;;;    Toyed with calling this function DCONS. Not quite analogous with CONS...
;;;    It doesn't create a DCONS object but instead modifies the two passed in.
;;;    
(defun dlink (previous next)
  (setf (next previous) next
        (previous next) previous))

;;;
;;;    There is an intimate connection between a cursor and its associated list.
;;;    Cursor will be attached to a list at the list's creation. However, the list
;;;    will be empty at that point, so there is no meaninful node to attach the
;;;    cursor to.
;;;    
;;;    [Always RESET cursor when modifying list (ADD/INSERT/DELETE)]
;;;    - Initializes first node  <- Only ADD/INSERT
;;;    - Index is potentially invalid anyway <- Any time NTH-DCONS modifies CURSOR!
;;;
(defclass dcursor ()
  ((list :initarg :list) ; Need to be able to find size
   (node :type (or null dcons))
   (index :initform 0))
  (:documentation "Cursor for circular doubly-linked list."))

(defmethod initialize-instance :after ((c dcursor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (list node) c
    (setf node (slot-value list 'store)))) ; Empty???

(defun initializedp (cursor)
  (not (null (slot-value cursor 'node))))

(defun reset (cursor)
  (with-slots (list node index) cursor
    (setf node (slot-value list 'store) ; ??? Not permanent. Can't put in constructor...
          index 0)))

(defun at-start-p (cursor)
  (or (not (initializedp cursor))
      (zerop (slot-value cursor 'index))))

(defun at-end-p (cursor)
  (with-slots (list index) cursor
    (or (not (initializedp cursor))
        (= index (1- (size list)))) ))

(defgeneric advance (cursor &optional step)
  (:documentation "Advance the cursor to the next node or ahead multiple nodes."))
(defmethod advance :around ((c dcursor) &optional step)
  (declare (ignore step))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod advance ((c dcursor) &optional (step 1))
  (assert (plusp step) () "STEP must be a positive value: ~A" step)
  (with-slots (list node index) c
    (loop repeat step
          do (incf index)
             (setf node (next node)))
    (setf index (mod index (size list)))) )

(defgeneric rewind (cursor &optional step)
  (:documentation "Rewind the cursor to the previous node or back multiple nodes."))
(defmethod rewind :around ((c dcursor) &optional step)
  (declare (ignore step))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod rewind ((c dcursor) &optional (step 1))
  (assert (plusp step) () "STEP must be a positive value: ~A" step)
  (with-slots (list node index) c
    (loop repeat step
          do (decf index)
             (setf node (previous node)))
    (setf index (mod index (size list)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Definitely not thread safe. CURSOR can only be used by one thread at a time???
;;;    
;;;    Test that cursor stays put when still valid???
;;;    
(defclass doubly-linked-list-old (list)
  ((store :initform nil)
   (count :initform 0)
   (head :documentation "Cursor starting from head of list.")
   (cursor :documentation "Floating cursor. May simplify access based on previous access."))
  (:documentation "Circular doubly-linked list."))

(defmethod initialize-instance :after ((l doubly-linked-list-old) &rest initargs)
  (declare (ignore initargs))
  (with-slots (cursor head) l
    (setf head (make-instance 'dcursor :list l) ; When to reset???
          cursor (make-instance 'dcursor :list l)))) 

(defun make-doubly-linked-list-old (&key (type t) (fill-elt nil))
  (make-instance 'doubly-linked-list-old :type type :fill-elt fill-elt))

(defmethod size ((l doubly-linked-list-old))
  (slot-value l 'count))

(defmethod emptyp ((l doubly-linked-list-old))
  (null (slot-value l 'store)))

;; (defmethod clear ((l doubly-linked-list-old))
;;   (with-slots (store count head cursor) l
;;     (setf store '() ; Memory leak?!
;;           count 0)
;;     (reset head)
;;     (reset cursor)))
          
(defmethod clear ((l doubly-linked-list-old))
  (unless (emptyp l)
    (with-slots (store count head cursor) l
      (loop repeat count
            for dcons = store then (next dcons)
            do (setf (previous dcons) nil))
      (setf (next store) nil
            store nil
            count 0)
      (reset head)
      (reset cursor))))

(defmethod iterator ((l doubly-linked-list-old))
  (make-instance 'doubly-linked-list-iterator :list l))

;; (defmethod contains ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (loop with iterator = (iterator l)
;;         until (done iterator)
;;         when (funcall test obj (current iterator))
;;           do (return t)
;;         else 
;;           do (next iterator)
;;         finally (return nil)))

;; (defmethod contains ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (loop repeat count
;;           for dcons = store then (next dcons)
;;           when (funcall test obj (content dcons))
;;             do (return t)
;;           finally (return nil))))

;;;
;;;    甲
;;;    
(defmethod contains ((l doubly-linked-list-old) obj &key (test #'eql))
  (with-slots (store count) l
    (labels ((find-obj (dcons i)
               (cond ((= i count) nil)
                     ((funcall test obj (content dcons)) t)
                     (t (find-obj (next dcons) (1+ i)))) ))
      (find-obj store 0))))

;;;
;;;    乙
;;;    
;; (defmethod contains ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (dotimes (i count nil)
;;       (when (funcall test obj (nth l i)) ; Reasonable due to cursor! Whoops... This takes 100X the time of previous. (1000X??!)
;;         (return t)))) )

;; (fill-list *dll* 10000)

;; ;;;
;; ;;;    甲
;; ;;;    
;; * (time (dotimes (i 10000) (contains *dll* i)))
;; Evaluation took:
;;   0.870 seconds of real time
;;   0.870819 seconds of total run time (0.870819 user, 0.000000 system)
;;   100.11% CPU
;;   3,135,080,156 processor cycles
;;   0 bytes consed
  
;; NIL

;; ;;;
;; ;;;    乙
;; ;;;    
;; * (time (dotimes (i 10000) (contains *dll* i)))
;; Evaluation took:
;;   1341.009 seconds of real time
;;   1340.972962 seconds of total run time (1340.952896 user, 0.020066 system)
;;   [ Run times consist of 0.117 seconds GC time, and 1340.856 seconds non-GC time. ]
;;   100.00% CPU
;;   8 lambdas converted
;;   4,827,636,027,846 processor cycles
;;   2,100,389,696 bytes consed
  
;; NIL

(defmethod add ((l doubly-linked-list-old) &rest objs)
  (unless (null objs)
    (with-slots (store count) l
      (labels ((add-nodes (head start elts)
                 (loop for dcons = start then (next dcons)
                       for i from 1
                       for elt in elts
                       do (dlink dcons (make-instance 'dcons :content elt))
                       finally (progn (dlink dcons head)
                                      (incf count i)))) )
        (let ((dcons (make-instance 'dcons :content (first objs))))
          (cond ((emptyp l) (setf store dcons))
                (t (let ((tail (previous store)))
                     (dlink tail dcons))))
          (add-nodes store dcons (rest objs)))) )))

(defmethod add :after ((l doubly-linked-list-old) &rest objs)
  (declare (ignore objs))
  (with-slots (cursor) l
    (with-slots (node) cursor
      (when (null node) ; Initialize cursor
        (reset cursor)))) )

;;;
;;;    This is the critical function to determine the quickest way
;;;    to locate the desired node.
;;;    Functions that call NTH-DCONS may accept a negative index. Such
;;;    an index must be offset before calling NTH-DCONS.
;;;
;;;
;;;    Rethink this! It seems to perform very poorly!!
;;;    
(defun nth-dcons-old (list i)
  (declare (integer i))
  (with-slots (store count head cursor) list
    (declare (integer count))
    (assert (typep i `(integer 0 (,count))) () "Invalid index: ~D" i)
    (cond ((emptyp list) (error "List is empty"))
          (t (with-slots ((c index) (c-node node)) cursor
               (declare (integer c))
               (with-slots ((h-node node)) head
                 (cond ((zerop i) store)
                       ((= i c) c-node)
                       ((< i (/ c 2))
                        (reset head)
                        (advance head i)
                        h-node)
                       ((< i c)
                        (rewind cursor (- c i))
                        c-node)
;                       ((<= (- i c) (/ (- count c) 2))
                       ((<= i (/ (+ count c) 2))
                        (advance cursor (- i c))
                        c-node)
                       ((zerop c)
                        (rewind cursor (- count i))
                        c-node)
                       (t (reset head)
                          (rewind head (- count i))
                          h-node)))) ))))

(defmethod insert ((l doubly-linked-list-old) (i integer) obj)
  (with-slots (store) l
    (let ((new-dcons (make-instance 'dcons :content obj)))
      (cond ((zerop i)
             (cond ((emptyp l) (dlink new-dcons new-dcons))
                   (t (dlink (previous store) new-dcons)
                      (dlink new-dcons store)))
             (setf store new-dcons))
            (t (let ((dcons (nth-dcons-old l i)))
                 (dlink (previous dcons) new-dcons)
                 (dlink new-dcons dcons)))) )))

(defmethod insert :after ((l doubly-linked-list-old) (i integer) obj)
  (declare (ignore obj))
  (with-slots (count cursor) l
    (incf count)
    (with-slots (node index) cursor
      (when (or (null node) 
                (<= 0 i index)
                (and (minusp i) (<= 0 (+ i count) index)))
        (reset cursor)))) )

(defmethod delete ((l doubly-linked-list-old) (i integer) &key &allow-other-keys)
  (with-slots (store) l
    (if (zerop i)
        (prog1 (content store)
          (if (eq store (next store))
              (setf store '())
              (let ((new-store (next store)))
                (dlink (previous store) new-store)
                (setf store new-store))))
        (let ((doomed (nth-dcons-old l i)))
          (prog1 (content doomed)
            (dlink (previous doomed) (next doomed)))) )))

(defmethod delete :after ((l doubly-linked-list-old) (i integer) &key &allow-other-keys)
  (declare (ignore i))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor))) ; NTH-DCONS moves cursor in most cases?

(defmethod nth ((l doubly-linked-list-old) (i integer))
  (with-slots (store count) l
    (content (nth-dcons-old l i))))

(defmethod (setf nth) (obj (l doubly-linked-list-old) (i integer))
  (with-slots (store) l
    (setf (content (nth-dcons-old l i)) obj)))

;;;
;;;    Copied from CONTAINS
;;;    
;; (defmethod index ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (loop with iterator = (iterator l)
;;         for i from 0
;;         until (done iterator)
;;         when (funcall test obj (current iterator))
;;           do (return i)
;;         else 
;;           do (next iterator)
;;         finally (return nil)))

;; (defmethod index ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (loop for i below count
;;           for dcons = store then (next dcons)
;;           when (funcall test obj (content dcons))
;;             do (return i)
;;           finally (return nil))))

(defmethod index ((l doubly-linked-list-old) obj &key (test #'eql))
  (with-slots (store count) l
    (labels ((find-obj (dcons i)
               (cond ((= i count) nil)
                     ((funcall test obj (content dcons)) i)
                     (t (find-obj (next dcons) (1+ i)))) ))
      (find-obj store 0))))

;; (defmethod index ((l doubly-linked-list-old) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (dotimes (i count nil)
;;       (when (funcall test obj (nth l i)) ; Reasonable due to cursor! Actually no...
;;         (return i)))) )

;; (defmethod slice ((l doubly-linked-list-old) (i integer) (n integer))
;;   (labels ((dsubseq (start end)
;;              (loop for dcons = (nth-dcons-old l start) then (next dcons)
;;                    for i from start below end
;;                    collect (content dcons))))
;;     (with-slots (type count fill-elt) l
;;       (let ((dll (make-doubly-linked-list-old :type type :fill-elt fill-elt))
;;             (slice (if (minusp i)
;;                        (let ((j (+ i count)))
;;                          (if (minusp j)
;;                              '()
;;                              (dsubseq j (min (+ j n) count))))
;;                        (dsubseq (min i count) (min (+ i n) count)))) )
;;         (apply #'add dll slice)
;;         dll))))

(defmethod slice ((l doubly-linked-list-old) (i integer) (n integer))
  (labels ((dsubseq (start end)
             (loop for dcons = (nth-dcons-old l start) then (next dcons)
                   for i from start below end
                   collect (content dcons))))
    (with-slots (type count fill-elt) l
      (let ((dll (make-doubly-linked-list-old :type type :fill-elt fill-elt)))
        (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))
        dll))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    Take two. Only one cursor.
;;;    
(defclass doubly-linked-list (linked-list)
  ((store :initform nil)
   (count :initform 0)
   (modification-count :initform 0)
   (cursor :documentation "Floating cursor. May simplify access based on previous access."))
  (:documentation "Circular doubly-linked list."))

(defmethod initialize-instance :after ((l doubly-linked-list) &rest initargs)
  (declare (ignore initargs))
  (with-slots (cursor) l
    (setf cursor (make-instance 'dcursor :list l)))) 

(defun make-doubly-linked-list (&key (type t) (fill-elt nil))
  (make-instance 'doubly-linked-list :type type :fill-elt fill-elt))

(defmethod size ((l doubly-linked-list))
  (slot-value l 'count))

(defmethod emptyp ((l doubly-linked-list))
  (null (slot-value l 'store)))

(defmethod clear ((l doubly-linked-list))
  (unless (emptyp l)
    (with-slots (store count cursor) l
      (loop repeat count
            for dcons = store then (next dcons)
            do (setf (previous dcons) nil))
      (setf (next store) nil
            store nil
            count 0)
      (reset cursor))))

(defmethod iterator ((l doubly-linked-list))
  (make-instance 'doubly-linked-list-iterator :list l))

(defmethod list-iterator ((l doubly-linked-list) &optional (start 0))
  (make-instance 'doubly-linked-list-list-iterator :list l :start start))

;; (defmethod contains ((l doubly-linked-list) obj &key (test #'eql))
;;   (loop with iterator = (iterator l)
;;         until (done iterator)
;;         when (funcall test obj (current iterator))
;;           do (return t)
;;         else 
;;           do (next iterator)
;;         finally (return nil)))

;; (defmethod contains ((l doubly-linked-list) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (loop repeat count
;;           for dcons = store then (next dcons)
;;           when (funcall test obj (content dcons))
;;             do (return t)
;;           finally (return nil))))

;;;
;;;    甲
;;;    
(defmethod contains ((l doubly-linked-list) obj &key (test #'eql))
  (with-slots (store count) l
    (labels ((find-obj (dcons i)
               (cond ((= i count) nil)
                     ((funcall test obj (content dcons)) (content dcons))
                     (t (find-obj (next dcons) (1+ i)))) ))
      (find-obj store 0))))

;;;
;;;    乙
;;;    
;; (defmethod contains ((l doubly-linked-list) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (dotimes (i count nil)
;;       (when (funcall test obj (nth l i)) ; Reasonable due to cursor! Whoops... This takes 100X the time of previous. (1000X??!)
;;         (return t)))) )

;; (fill-list *dll* 10000)

;; ;;;
;; ;;;    甲
;; ;;;    
;; * (time (dotimes (i 10000) (contains *dll* i)))
;; Evaluation took:
;;   0.836 seconds of real time
;;   0.836248 seconds of total run time (0.836248 user, 0.000000 system)
;;   100.00% CPU
;;   3,010,721,134 processor cycles
;;   0 bytes consed
  
;; NIL

;; ;;;
;; ;;;    乙 Better but still not great...
;; ;;;    
;; * (time (dotimes (i 10000) (contains *dll* i)))
;; Evaluation took:
;;   25.207 seconds of real time
;;   25.206613 seconds of total run time (25.182618 user, 0.023995 system)
;;   [ Run times consist of 0.562 seconds GC time, and 24.645 seconds non-GC time. ]
;;   100.00% CPU
;;   90,746,696,714 processor cycles
;;   10,400,560,320 bytes consed
  
;; NIL

(defmethod add ((l doubly-linked-list) &rest objs)
  (unless (null objs)
    (with-slots (store count) l
      (labels ((add-nodes (head start elts)
                 (loop for dcons = start then (next dcons)
                       for i from 1
                       for elt in elts
                       do (dlink dcons (make-instance 'dcons :content elt))
                       finally (progn (dlink dcons head)
                                      (incf count i)))) )
        (let ((dcons (make-instance 'dcons :content (first objs))))
          (cond ((emptyp l) (setf store dcons))
                (t (let ((tail (previous store)))
                     (dlink tail dcons))))
          (add-nodes store dcons (rest objs)))) )))

(defmethod add :after ((l doubly-linked-list) &rest objs)
  (declare (ignore objs))
  (with-slots (cursor) l
    (unless (initializedp cursor)
      (reset cursor))))

;;;
;;;    This is the critical function to determine the quickest way
;;;    to locate the desired node.
;;;    Functions that call NTH-DCONS may accept a negative index. Such
;;;    an index must be offset before calling NTH-DCONS.
;;;
(defun nth-dcons (list i)
  (declare (integer i))
  (with-slots (store count cursor) list
    (declare (integer count))
;    (assert (typep i `(integer 0 (,count))) () "Invalid index: ~D" i)
    (assert (and (>= i 0) (< i count)) () "Invalid index: ~D" i)
    (cond ((emptyp list) (error "List is empty"))
          (t (with-slots ((c index) node) cursor
               (declare (integer c))
               (cond ((zerop i) store)
                     ((= i c) node)
                     ((< i (/ c 2))
                      (reset cursor)
                      (advance cursor i)
                      node)
                     ((< i c)
                      (rewind cursor (- c i))
                      node)
                     ((<= i (/ (+ count c) 2))
                      (advance cursor (- i c))
                      node)
                     (t (reset cursor)
                        (rewind cursor (- count i))
                        node)))) )))

(defmethod insert ((l doubly-linked-list) (i integer) obj)
  (insert-before l (nth-dcons l i) obj))

(defmethod insert :after ((l doubly-linked-list) (i integer) obj)
  (declare (ignore obj))
  (with-slots (count cursor) l
;    (incf count)
    (with-slots (index) cursor
      (when (or (not (initializedp cursor))
                (<= 0 i index)
                (and (minusp i) (<= 0 (+ i count) index)))
        (reset cursor)))) )

(defmethod insert-before ((l doubly-linked-list) node obj)
  (with-slots (store) l
    (let ((new-dcons (make-instance 'dcons :content obj)))
      (dlink (previous node) new-dcons)
      (dlink new-dcons node)
      (when (eq store node)
        (setf store new-dcons)))) )

(defmethod insert-before :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count cursor) l
    (incf count)
    (with-slots (index) cursor
      (incf index))))

(defmethod insert-after ((l doubly-linked-list) node obj)
  (let ((new-dcons (make-instance 'dcons :content obj)))
    (dlink node new-dcons)
    (dlink new-dcons (next node))))

(defmethod insert-after :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

;; (defmethod delete ((l doubly-linked-list) (i integer))
;;   (with-slots (store) l
;;     (if (zerop i)
;;         (prog1 (content store)
;;           (if (eq store (next store))
;;               (setf store '())
;;               (let ((new-store (next store)))
;;                 (dlink (previous store) new-store)
;;                 (setf store new-store))))
;;         (let ((doomed (nth-dcons l i)))
;;           (prog1 (content doomed)
;;             (dlink (previous doomed) (next doomed)))) )))

;; (defmethod delete :after ((l doubly-linked-list) (i integer))
;;   (declare (ignore i))
;;   (with-slots (count cursor) l
;;     (decf count)
;;     (reset cursor))) ; NTH-DCONS moves cursor in most cases?

(defmethod delete ((l doubly-linked-list) (i integer) &key &allow-other-keys)
  (delete l (nth-dcons l i)))

;;;
;;;    Confirm that DOOMED is actually a node in this list!
;;;    - Smuggling in some fabricated DCONS could snip out multiple nodes depending
;;;      on what its NEXT and PREVIOUS links pointed to.
;;;      
;;;    This method needs to be absolutely private! Risky to split an object up like this...
;;;    
(defmethod delete ((l doubly-linked-list) (doomed dcons) &key &allow-other-keys)
  (with-slots (store) l
    (assert (and (eq doomed (next (previous doomed))) ; This still doesn't prove that DOOMED is in L! But it can't damage L if it doesn't belong... Only decrements L's COUNT.
                 (eq doomed (previous (next doomed))))
            ()
            "This node ~D does not belong here!"
            doomed)
    (prog1 (content doomed)
      (cond ((and (eq doomed store) (eq store (next store))) ; First test confirms that DOOMED is part of L. Otherwise possible to remove last remaining node of L with junk DOOMED node.
             (setf store '()))
;            ((eq store (next store)) (error "Whaa?"))
            (t (dlink (previous doomed) (next doomed))
               (when (eq doomed store)
                 (setf store (next store)))) ))))

;(defmethod delete :after ((l doubly-linked-list) (i integer))
(defmethod delete :after ((l doubly-linked-list) (doomed dcons) &key &allow-other-keys)
  (declare (ignore doomed))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor))) ; NTH-DCONS moves cursor in most cases?

(defmethod nth ((l doubly-linked-list) (i integer))
  (with-slots (store count) l
    (content (nth-dcons l i))))

(defmethod (setf nth) (obj (l doubly-linked-list) (i integer))
  (with-slots (store) l
    (setf (content (nth-dcons l i)) obj)))

(defmethod index ((l doubly-linked-list) obj &key (test #'eql))
  (with-slots (store count) l
    (labels ((find-obj (dcons i)
               (cond ((= i count) nil)
                     ((funcall test obj (content dcons)) i)
                     (t (find-obj (next dcons) (1+ i)))) ))
      (find-obj store 0))))

;; (defmethod index ((l doubly-linked-list) obj &key (test #'eql))
;;   (with-slots (store count) l
;;     (dotimes (i count nil)
;;       (when (funcall test obj (nth l i)) ; Reasonable due to cursor! Actually no...
;;         (return i)))) )

(defmethod slice ((l doubly-linked-list) (i integer) (n integer))
  (labels ((dsubseq (start end)
             (loop for i from start below end
                   for dcons = (nth-dcons l start) then (next dcons)
                   collect (content dcons))))
    (with-slots (count) l
      (let ((dll (make-doubly-linked-list :type (type l) :fill-elt (fill-elt l))))
        (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))
        dll))))

(defclass doubly-linked-list-iterator (iterator)
  ((cursor)
   (sealed-for-your-protection :initform t)))

(defmethod initialize-instance :after ((i doubly-linked-list-iterator) &rest initargs &key list)
  (declare (ignore initargs))
  (with-slots (cursor) i
    (setf cursor (make-instance 'dcursor :list list))))

(defmethod current ((i doubly-linked-list-iterator))
  (with-slots (cursor) i
    (content (slot-value cursor 'node))))

(defmethod next ((i doubly-linked-list-iterator))
  (with-slots (cursor sealed-for-your-protection) i
    (cond ((done i) nil)
          (t (advance cursor)
             (setf sealed-for-your-protection nil)
             (if (done i)
                 nil
                 (current i)))) ))

(defmethod done ((i doubly-linked-list-iterator))
  (with-slots (cursor sealed-for-your-protection) i
    (or (not (initializedp cursor))
        (and (not sealed-for-your-protection) (at-start-p cursor)))) )

;;;
;;;    HASH-TABLE-LIST
;;;    
;;;    Oops. This is not as simple as a stack or queue. Since elements can be inserted or deleted
;;;    the deli counter model falls apart. An insertion or deletion could require that the mapping
;;;    of keys to elements be recomputed--similar to shifting elements in an array.
;;;
(defclass hash-table-list (list)
  ((store :initform (make-hash-table))
   (modification-count :initform 0)))

(defmethod size ((l hash-table-list))
  (with-slots (store) l
    (hash-table-count store)))

(defmethod emptyp ((l hash-table-list))
  (zerop (size l)))

(defmethod clear ((l hash-table-list))
  (with-slots (store modification-count) l
    (incf modification-count)
    (clrhash store)))

(defmethod iterator ((l hash-table-list))
  (make-instance 'random-access-list-iterator :list l))

(defmethod list-iterator ((l hash-table-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l hash-table-list) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
      (when (funcall test obj (gethash i store))
        (return (gethash i store)))) ))

(defmethod add ((l hash-table-list) &rest objs)
  (unless (null objs)
    (with-slots (store modification-count) l
      (incf modification-count) ; Only 1?
      (loop for i from (size l)
            for obj in objs
            do (setf (gethash i store) obj)))) )

(defmethod insert ((l hash-table-list) (i integer) obj)
  (with-slots (store modification-count) l
    (incf modification-count)
    (loop for j from (size l) above i
          do (setf (gethash j store) (gethash (1- j) store)))
    (setf (gethash i store) obj)))
                   
(defmethod delete ((l hash-table-list) (i integer) &key &allow-other-keys)
  (with-slots (store modification-count) l
    (incf modification-count)
    (let ((count (size l)))
      (prog1 (gethash i store)
        (loop for j from i below (1- count)
              do (setf (gethash j store) (gethash (1+ j) store)))
        (remhash (1- count) store)))) )

(defmethod nth ((l hash-table-list) (i integer))
  (with-slots (store) l
    (gethash i store)))

(defmethod (setf nth) (obj (l hash-table-list) (i integer))
  (with-slots (store) l
    (setf (gethash i store) obj)))

(defmethod index ((l hash-table-list) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
      (when (funcall test obj (gethash i store))
        (return i)))) )

(defmethod slice ((l hash-table-list) (i integer) (n integer))
  (with-slots (store) l
    (let ((htl (make-instance 'hash-table-list :type (type l) :fill-elt (fill-elt l)))
          (count (size l)))
      (apply #'add htl (loop for k from (min i count) below (min (+ i n) count) collect (gethash k store)))
      htl)))

;;;
;;;    PERSISTENT-LIST
;;; 
(defclass persistent-list (list)
  ((store :initform '() :initarg :store)
   (count :initform 0 :initarg :count))) ; Should we trust this??

(defmethod print-object ((l persistent-list) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "(")
    (unless (emptyp l)
      (let ((iterator (iterator l)))
        (format stream "~A" (current iterator))
        (loop for i = (next iterator) then (next i)
              until (done i)
              do (format stream " ~A" (current i)))) )
    (format stream ")")))

(defun make-persistent-list (&key (type t) (fill-elt nil))
  (make-instance 'persistent-list :type type :fill-elt fill-elt))

(defmethod size ((l persistent-list))
  (slot-value l 'count))

(defmethod emptyp ((l persistent-list))
  (null (slot-value l 'store)))

(defmethod clear ((l persistent-list))
  (make-instance 'persistent-list :type (type l) :fill-elt (fill-elt l)))

(defmethod iterator ((l persistent-list))
  (make-instance 'persistent-list-iterator :list l))

;;list-iterator...

(defmethod contains ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

(defmethod add ((l persistent-list) &rest objs)
  (if (null objs)
      l
      (with-slots (store count) l
        (loop for i from 0
              for elt in objs
              collect elt into elts
              finally (return (make-instance 'persistent-list 
                                             :store (append store elts) 
                                             :count (+ i count)
                                             :type (type l)
                                             :fill-elt (fill-elt l)))) )))

(defmethod insert ((l persistent-list) (i integer) obj)
  (with-slots (store count) l
    (make-instance 'persistent-list
                   :store (loop for j from 0 below i
                                for cons on store
                                collect (first cons) into head
                                finally (return (nconc head (cons obj cons))))
                   :count (1+ count)
                   :type (type l)
                   :fill-elt (fill-elt l))))

(defmethod delete ((l persistent-list) (i integer) &key &allow-other-keys)
  (with-slots (store count) l
    (cond ((zerop i) (make-instance 'persistent-list
                                    :store (rest store)
                                    :count (1- count)
                                    :type (type l)
                                    :fill-elt (fill-elt l)))
          ((< i count) (make-instance 'persistent-list
                                    :store (loop for j below i
                                                 for elt in store
                                                 for tail on (rest store)
                                                 collect elt into elts
                                                 finally (return (nconc elts (rest tail))))
                                    :count (1- count)
                                    :type (type l)
                                    :fill-elt (fill-elt l)))
          (t l)))) ; Short-circuited by :AROUND method??!?!?!?

(defmethod nth ((l persistent-list) (i integer))
  (with-slots (store) l
    (cl:nth i store)))

;;;
;;;    SETF method need not actually set anything???
;;;    Simply used for value...
;;;    
(defmethod (setf nth) (obj (l persistent-list) (i integer))
  (with-slots (store count) l
    (make-instance 'persistent-list
                   :store (loop for j below i
                             for elt in store
                             for tail on (rest store)
                             collect elt into elts
                             finally (return (nconc elts (cons obj (rest tail)))) )
                   :count count
                   :type (type l)
                   :fill-elt (fill-elt l))))

(defmethod index ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

(defmethod slice ((l persistent-list) (i integer) (n integer))
  (with-slots (store count) l
    (let* ((start (min i count))
           (end (min (+ i n) count))
           (count (- end start)))
      (make-instance 'persistent-list
                     :store (subseq store start end)
                     :count count
                     :type (type l)
                     :fill-elt (fill-elt l)))) )

;;;
;;;    Don't need to hold onto LIST after initialization?
;;;    CURSOR added to avoid manipulating LIST directly...
;;;    
;;;
;;;    Identical to SINGLY-LINKED-LIST-ITERATOR?!
;;;    Iterator should be persistent too?!? <-----------------------------------------------------------------------------
;;;    
(defclass persistent-list-iterator (iterator)
  ((cursor)))

(defmethod initialize-instance :after ((i persistent-list-iterator) &rest initargs &key (list nil listp) (store nil storep))
  (declare (ignore initargs))
  (with-slots (cursor) i
    (cond (listp (setf cursor (slot-value list 'store))) ; Inappropriate access?
          (storep (setf cursor store))
          (t (error "Missing list")))) )

(defmethod current ((i persistent-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

;; (defmethod next ((i persistent-list-iterator))
;;   (with-slots (cursor) i
;;     (cond ((done i) nil)
;;           (t (cl:pop cursor)
;;              (if (done i)
;;                  nil
;;                  (current i)))) ))

(defmethod next ((i persistent-list-iterator))
  (with-slots (cursor) i
    (cond ((done i) i)
          (t (make-instance 'persistent-list-iterator :store (rest cursor)))) ))

(defmethod done ((i persistent-list-iterator))
  (with-slots (cursor) i
    (null cursor)))

;;;
;;;    LIST-ITERATOR
;;;
;;;    This is a hybrid of Fox's definition and java.util.ListIterator:
;;;    1. It is not a sub-interface of ITERATOR.
;;;    2. The cursor conceptually points to an element rather than between as in Java.
;;;       In other words, there is a "current" element and index.
;;;
;;;    A list and its list iterator have an asymmetric relationship. The list can stand
;;;    alone of any list iterator, but the list iterator is intimately tied to its list.
;;;    There may be multiple list iterators associated with a given list.
;;;
;;;    A list iterator will detect structural changes to the host list that occur independently
;;;    of the list iterator:
;;;    1. Directly modifying the list (ADD/INSERT/DELETE/INSERT-BEFORE/INSERT-AFTER)
;;;    2. Changes made by another list iterator (REMOVE/ADD-BEFORE/ADD-AFTER)
;;;    Such changes invalidate the list iterator, and it signals an error.
;;;
;;;    The list iterator is intended for linked lists that do not have efficient random
;;;    accessibility. They allow directly traversing from one known node to the next.
;;;    
;;;    Empty list => empty LIST-ITERATOR (:AROUND methods???)
;;;
;;;    RESET method?? (CLEAR?)
;;;
(defclass list-iterator ()
  ()
  (:documentation "External iterator for a list. May traverse in either direction."))

(defmethod type ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement TYPE"))

;;; True for single-element list too!!!
;; (defmethod emptyp ((i list-iterator))
;;   (not (or (has-next i)
;;            (has-previous i))))
(defmethod emptyp ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement EMPTYP"))

;;
;;    Eliminate CURRENT in favor or NEXT/PREVIOUS? (Both move cursor and return elt. Error if at end?)
;;    

(defmethod current :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod current ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement CURRENT"))

(defgeneric current-index (iterator)
  (:documentation "Returns index of the current element of the iterator traversal."))
(defmethod current-index :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod current-index ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement CURRENT-INDEX"))

(defgeneric (setf current) (obj list-iterator)
  (:documentation "Assign the object at the current iterator position."))
(defmethod (setf current) :around (obj (i list-iterator))
  (cond ((not (typep obj (type i))) (error "~A is not of type ~A" obj (type i)))
        ((emptyp i) (error "List is empty"))
        (t (call-next-method))))
(defmethod (setf current) (obj (i list-iterator))
  (declare (ignore i obj))
  (error "list iterator does not implement (SETF CURRENT)"))

(defmethod next :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod next ((i list-iterator))
  (declare (ignore i))
  (error "iterator does not implement NEXT"))

(defgeneric previous (iterator)
  (:documentation "Rewinds iterator to the previous element of the traversal."))
(defmethod previous :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod previous ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement PREVIOUS"))

(defgeneric has-next (iterator)
  (:documentation "Is there a next element for the iterator?"))
(defmethod has-next ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement HAS-NEXT"))

(defgeneric has-previous (iterator)
  (:documentation "Is there a previous element for the iterator?"))
(defmethod has-previous ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement HAS-PREVIOUS"))

;;;
;;;    "Next" elt becomes current unless at end of list.
;;;    
(defgeneric remove (iterator)
  (:documentation "Remove the current element of the iterator from the list."))
(defmethod remove :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod remove ((i list-iterator))
  (declare (ignore i))
  (error "list iterator does not implement REMOVE"))

;;; ADD-BEFORE/ADD-AFTER  what about empty list? No CURRENT to establish "before"/"after"
;;; Just ADD? Java cursor lies between elts--ADD is more meaningful. For me there would
;;; be no way to just ADD to front of list (rather than after first elt).
;;; 
(defgeneric add-before (iterator obj)
  (:documentation "Add an element to the list before the current element of the iterator."))
(defmethod add-before :around ((i list-iterator) obj)
  (if (not (typep obj (type i)))
      (error "~A is not of type ~A" obj (type i))
      (call-next-method)))
(defmethod add-before ((i list-iterator) obj)
  (declare (ignore i obj))
  (error "list iterator does not implement ADD-BEFORE"))

(defgeneric add-after (iterator obj)
  (:documentation "Add an element to the list after the current element of the iterator."))
(defmethod add-after :around ((i list-iterator) obj)
  (if (not (typep obj (type i)))
      (error "~A is not of type ~A" obj (type i))
      (call-next-method)))
(defmethod add-after ((i list-iterator) obj)
  (declare (ignore i obj))
  (error "list iterator does not implement ADD-AFTER"))

;;;
;;;    Used by ARRAY-LIST and HASH-TABLE-LIST
;;;    
;;;
;;;    RANDOM-ACCESS-LIST-LIST-ITERATOR
;;;
(defclass random-access-list-list-iterator (list-iterator)
  ((list :initarg :list)
   (cursor :type integer)
   (expected-modification-count :type integer)))

(defmethod initialize-instance :after ((i random-access-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list cursor expected-modification-count) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (setf cursor start
          expected-modification-count (slot-value list 'modification-count))))

(defmethod type ((i random-access-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i random-access-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

(defmethod current :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current ((i random-access-list-list-iterator))
  (with-slots (list cursor) i
    (nth list cursor)))

(defmethod current-index :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current-index ((i random-access-list-list-iterator))
  (slot-value i 'cursor))

(defmethod (setf current) :around (obj (i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod (setf current) (obj (i random-access-list-list-iterator))
  (with-slots (list) i
    (setf (nth list (current-index i)) obj)))

(defmethod next :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod next ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-next i)
           (incf cursor)
           (current i))
          (t nil))))

(defmethod previous :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod previous ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-previous i)
           (decf cursor)
           (current i))
          (t nil))))

(defmethod has-next :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-next ((i random-access-list-list-iterator))
  (with-slots (list cursor) i
    (< cursor (1- (size list)))) )

(defmethod has-previous :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-previous ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (> cursor 0)))

(defmethod remove :around ((i random-access-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod remove ((i random-access-list-list-iterator))
  (with-slots (list cursor expected-modification-count) i
    (let ((index cursor))
      (when (and (has-previous i)
                 (not (has-next i)))
        (decf cursor))
      (prog1 (delete list index)
        (incf expected-modification-count)))) )

(defmethod add-before :around ((i random-access-list-list-iterator) obj)
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod add-before ((i random-access-list-list-iterator) obj)
  (with-slots (list cursor expected-modification-count) i
    (cond ((emptyp i) (add list obj))
          (t (insert list cursor obj)
             (incf cursor)))
    (incf expected-modification-count)))

(defmethod add-after :around ((i random-access-list-list-iterator) obj)
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod add-after ((i random-access-list-list-iterator) obj)
  (with-slots (list cursor expected-modification-count) i
    (cond ((emptyp i) (add list obj))
          (t (insert list (1+ cursor) obj)))
    (incf expected-modification-count)))

;;;
;;;    SINGLY-LINKED-LIST-LIST-ITERATOR
;;;
;;;    HISTORY is a stack that captures all of the tails when moving forward
;;;    so that PREVIOUS can work its way backward.
;;;
;;;    CURSOR is initially detached if created on an empty list. Many methods have :BEFORE methods to initialize
;;;    once the list is populated. It is assumed that the cursor will spring to life at index 0, e.g., calling
;;;    REMOVE will remove the 0th element.
;;;    
(defclass singly-linked-list-list-iterator (list-iterator)
  ((list :initarg :list)
   (index :type integer :initform 0)
   (cursor :type (or null cons))
   (history :initform (make-instance 'linked-stack))
   (expected-modification-count :type integer)))

(defmethod initialize-instance :after ((i singly-linked-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list index cursor expected-modification-count) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (setf cursor (slot-value list 'store) ; Inappropriate access?
          expected-modification-count (slot-value list 'modification-count))
    (loop repeat start do (next i)))) ; Build initial history

;;;
;;;    CURSOR may be detached when:
;;;    1. List iterator is created on empty list
;;;    2. List becomes empty
;;;    
(defun initialize-cursor (list-iterator)
  (with-slots (list cursor) i
    (setf cursor (slot-value list 'store))))
  
(defmethod type ((i singly-linked-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i singly-linked-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

(defmethod current :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod current ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod current-index :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod current-index :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod current-index ((i singly-linked-list-list-iterator))
  (slot-value i 'index))

(defmethod (setf current) :around (obj (i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod (setf current) :before (obj (i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod (setf current) (obj (i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (setf (first cursor) obj)))

;;;
;;;    CURSOR is NIL but STORE is not?? (List iterator created with empty list?)
;;;    
(defmethod next :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod next :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod next ((i singly-linked-list-list-iterator))
  (with-slots (cursor index history) i
    (cond ((has-next i)
           (push history cursor)
           (cl:pop cursor)
           (incf index)
           (current i))
          (t nil))))

;;;
;;;    This is the worst one. Due to links in only one direction, we have to partially traverse
;;;    from the head each time.
;;;    
;; (defmethod previous ((i singly-linked-list-list-iterator))
;;   (with-slots (list cursor index) i
;;     (cond ((has-previous i)
;;            (let ((current cursor))
;;              (decf index)
;;              (setf cursor (slot-value list 'store))
;;              (loop until (eq (rest cursor) current)
;;                    do (cl:pop cursor))
;;              (current i)))
;;           (t nil))))

(defmethod previous :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod previous :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod previous ((i singly-linked-list-list-iterator))
  (with-slots (cursor index history) i
    (cond ((has-previous i)
           (setf cursor (pop history))
           (decf index)
           (current i))
          (t nil))))

(defmethod has-next :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-next :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod has-next ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (null (rest cursor)))) )

(defmethod has-previous :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-previous :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod has-previous ((i singly-linked-list-list-iterator))
  (with-slots (list cursor) i
    (not (eq cursor (slot-value list 'store)))) )

;; (defmethod remove ((i singly-linked-list-list-iterator))
;;   (with-slots (list cursor index) i
;;     (let ((current index))
;;       (if (has-next i)
;;           (cl:pop cursor)
;;           (previous i))
;;       (delete list current))))


;;;    Remove last elt --> Update cursor!
;; Must invalidate history!
;; (defmethod remove ((i singly-linked-list-list-iterator))
;;   (with-slots (list cursor index history) i
;;     (prog1 (first cursor)
;;       ;; (if (eq cursor (slot-value list 'store))
;;       ;;     (delete list 0)
;;       ;;     (delete list (peek history)))
;;       (if (emptyp history) ; (not (has-previous i)) ??
;;           (delete list cursor)
;; ;          (delete list 0)
;;           (delete list (peek history) :parent t))
;;       (unless (emptyp list)
;;         (cl:pop cursor)
;;         (when (null cursor)
;;           (setf cursor (pop history))
;;           (decf index)))) ))
;            (setf cursor (rest (peek history)))) ))))
        
      ;; (unless (emptyp list)
      ;;   (rebuild-history i)
      ;;   (when (and (has-previous i)
      ;;              (not (has-next i)))
      ;;     (previous i)))) ))

(defmethod remove :around ((i singly-linked-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod remove :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod remove ((i singly-linked-list-list-iterator))
  (with-slots (list cursor index history expected-modification-count) i
    (prog1 (first cursor)
      (cond ((emptyp history)
             (delete list cursor)
             (when (emptyp list)
               (setf cursor '())))
            (t (delete list (peek history) :parent t)
               (unless (emptyp list)
                 (cl:pop cursor)
                 (when (null cursor)
                   (setf cursor (pop history))
                   (decf index)))) )
      (incf expected-modification-count))))

(defmethod add-before :around ((i singly-linked-list-list-iterator) obj)
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod add-before ((i singly-linked-list-list-iterator) obj)
  (with-slots (list cursor index history expected-modification-count) i
    (cond ((emptyp i)
           (add list obj)
           (initialize-cursor i))
          (t (insert-before list cursor obj)
             (incf expected-modification-count)
             (let ((current index))
               (setf cursor (slot-value list 'store)
                     index 0)
               (clear history)
               (loop repeat (1+ current) do (next i)))) ))) ; Rebuild history

(defmethod add-after :around ((i singly-linked-list-list-iterator) obj)
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod add-after ((i singly-linked-list-list-iterator) obj)
  (with-slots (list cursor index expected-modification-count) i
    (cond ((emptyp i)
           (add list obj)
           (initialize-cursor i))
          (t (insert-after list cursor obj)))
    (incf expected-modification-count)))

;;;
;;;    DOUBLY-LINKED-LIST-LIST-ITERATOR
;;;    
(defclass doubly-linked-list-list-iterator (list-iterator)
  ((list :initarg :list)
   (cursor :type dcursor)))

(defmethod initialize-instance :after ((i doubly-linked-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list cursor) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (setf cursor (make-instance 'dcursor :list list))
    (unless (zerop start)
      (advance cursor start))))

(defmethod type ((i doubly-linked-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i doubly-linked-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

(defmethod current :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod current ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (content (slot-value cursor 'node))))

(defmethod current-index :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod current-index ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (slot-value cursor 'index)))

(defmethod (setf current) (obj (i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (with-slots (node) cursor
      (setf (content node) obj))))

(defmethod next ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-next i)
           (advance cursor)
           (current i))
          (t nil))))

(defmethod previous ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-previous i)
           (rewind cursor)
           (current i))
          (t nil))))

(defmethod has-next ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-end-p cursor))))

(defmethod has-previous ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-start-p cursor))))

;; (defmethod remove ((i doubly-linked-list-list-iterator))
;;   (with-slots (list cursor) i
;;     (with-slots (node index) cursor
;;       (let ((current-index index))
;;         (cond ((has-next i) (setf node (next node))) ; Half advance...
;;               ((has-previous i) (rewind cursor)))
;;         (prog1 (delete list current-index)
;;           (when (emptyp i)
;;             (reset cursor)))) ))) ; iterator is dead ... ADD may resuscitate?

(defmethod remove ((i doubly-linked-list-list-iterator))
  (with-slots (list cursor) i
    (with-slots (node) cursor
      (let ((current-node node))
        (cond ((has-next i) (setf node (next node))) ; Half advance...
              ((has-previous i) (rewind cursor)))
        (prog1 (delete list current-node)
          (when (emptyp i)
            (reset cursor)))) ))) ; iterator is dead ... ADD may resuscitate?

(defmethod add-before ((i doubly-linked-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i)
           (add list obj)
           (reset cursor))
          (t (with-slots (index node) cursor
               (insert-before list node obj)
               (incf index)))) ))

(defmethod add-after ((i doubly-linked-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i)
           (add list obj)
           (reset cursor))
          (t (with-slots (node) cursor
               (insert-after list node obj)))) ))
