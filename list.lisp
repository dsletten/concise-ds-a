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
;;;;   List iterator is still valid after its ADD/REMOVE methods are called... How about list methods? No
;;;;
;;;;   Be cautious with this pattern:
;;;;     (with-slots (list cursor) i
;;;;       (with-slots (index node) cursor
;;;;
;;;;   The first level of WITH-SLOTS is fine. Same as having access to private instance vars in Java class.
;;;;   But the 2nd level is reaching into some other object.
;;;;

;;;;
;;;;    TODO:
;;;;    Examine ARRAY-LIST-X DELETE tweaks.
;;;;    Fix SINGLY-LINKED-LIST iterator/list-iterator STORE vs. FRONT!!!
;;;;      ✓ Encapsulated by closure!
;;;;    ✓ Refactor ERROR messages...
;;;;    - Other API methods: APPEND, REVERSE, ...
;;;;


;;;;
;;;;    Implement ITERATOR/LIST-ITERATOR as slots on a list instance? Slot would be factory
;;;;    function to produce the desired iterator...
;;;;    



(in-package :containers)

;;;
;;;    Where do these belong?
;;;

;;;
;;;    Make copy of current node. Surgically update current node to
;;;    become "previous" node in place.
;;;    
(defgeneric splice-before (node obj)
  (:documentation "Splice OBJ into chain of nodes before NODE."))
(defmethod splice-before ((node cons) obj)
  (let ((copy (cons (first node) (rest node))))
    (setf (first node) obj
          (rest node) copy)))

(defgeneric splice-after (node obj)
  (:documentation "Splice OBJ into chain of nodes after NODE."))
(defmethod splice-after ((node cons) obj)
  (let ((tail (cons obj (rest node))))
    (setf (rest node) tail)))

(defgeneric excise-node (doomed)
  (:documentation "Surgically remove the doomed node from the list."))
(defmethod excise-node ((doomed cons))
  (let ((content (first doomed))
        (saved (rest doomed)))
    (prog1 content
      (cond ((null saved) ; Can't delete last node
             (error "Target node must have non-nil next node"))
            (t (setf (first doomed) (first saved)
                     (rest doomed) (rest saved)))) )))

(defgeneric excise-child (parent)
  (:documentation "Surgically remove the child of the given node from the list."))
(defmethod excise-child ((parent cons))
  (let ((child (rest parent)))
    (if (null child)
        (error "Parent must have child node")
        (prog1 (first child)
          (setf (rest parent) (rest child)))) ))





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
          do (format stream "~S" (current i))
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
;; (defmethod equals ((l1 list) (l2 persistent-list) &key (test #'eql))
;;   (equals l2 l1 :test #'(lambda (x y) (funcall test y x))))
;; (defmethod equals ((l1 persistent-list) (l2 list) &key (test #'eql))
;;   (if (= (size l1) (size l2))
;;       (let ((i2 (iterator l2)))
;;         (loop for i1 = (iterator l1) then (next i1)
;;               do (cond ((done i1) (return (done i2)))
;;                        ((done i2) (return nil))
;;                        ((funcall test (current i1) (current i2)) (next i2))
;;                        (t (return nil)))) )
;;       nil))
;; (defmethod equals ((l1 persistent-list) (l2 persistent-list) &key (test #'eql))
;;   (if (= (size l1) (size l2))
;;       (loop for i1 = (iterator l1) then (next i1)
;;             for i2 = (iterator l2) then (next i2)
;;             do (cond ((done i1) (return (done i2)))
;;                      ((done i2) (return nil))
;;                      ((not (funcall test (current i1) (current i2))) (return nil))))
;;       nil))

(defmethod each ((l list) op)
  (let ((i (iterator l)))
    (loop until (done i)
          do (funcall op (current i))
             (next i))))

(defgeneric list-iterator (list &optional start)
  (:documentation "Returns a list iterator for the list."))
(defmethod list-iterator ((l list) &optional start)
  (declare (ignore l start))
  (error "LIST does not implement LIST-ITERATOR"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Structural modification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    See semantics of https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/Collection.html#add(E)
;;;    Regarding return value. --Optionally return the list itself??
;;;    - Should be part of COLLECTION interface?
;;;    
(defgeneric add (list &rest objs)
  (:documentation "Add the objects to the end of the list."))
(defmethod add :around ((l list) &rest objs)
  (if (every #'(lambda (obj) (typep obj (type l))) objs)
      (call-next-method)
      (error "Type mismatch with OBJS")))
(defmethod add ((l list) &rest objs)
  (declare (ignore l objs))
  (error "LIST does not implement ADD"))

(defun extend-list (list i obj)
  (apply #'add list (loop repeat (1+ (- i (size list)))
                          for tail = (cl:list obj) then (cons (fill-elt list) tail)
                          finally (return tail))))

;;;
;;;    INSERT multiples like ADD?
;;;    Negative index only makes sense for non-empty list.
;;;    - Fox is not clear what is supposed to happen if negative index is
;;;      beyond start of list. The obvious choice is to not insert anything.
;;;      The less obvious choice involves PERSISTENT-LISTs. What should be returned?
;;;      I am modelling INSERT after DELETE, which simply returns the list "as is".
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
  (error "LIST does not implement INSERT"))

;;;
;;;    Fox's spec simply states that with an out-of-bound index to DELETE
;;;    "nothing is changed". I added the additional (inconsistent) precondition
;;;    that it is an error to delete from an empty list.
;;;    This requires duplication in the :AROUND method for PERSISTENT-LIST
;;;    and spills over to the unit tests (TEST-CONSTRUCTOR).
;;;    
(defgeneric delete (list i)
  (:documentation "Delete the object at the given index."))
(defmethod delete :around ((l list) (i integer))
  (cond ((emptyp l) (error "List is empty")) ; This is wrong????
        ((minusp i)
         (let ((j (+ i (size l)))) ; {-n, ..., -1} -> {0, ..., n-1}
           (unless (minusp j)
             (delete l j))))
        ((< i (size l)) (call-next-method))))
(defmethod delete ((l list) (i integer))
  (declare (ignore l i))
  (error "LIST does not implement DELETE"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (error "LIST does not implement NTH"))

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
  (error "LIST does not implement (SETF NTH)"))

(defgeneric index (list obj &key test)
  (:documentation "Determine index of the object if present in the list."))
(defmethod index :around ((l list) obj &key test)
  (declare (ignore test)) ; Why is this needed?!
  (if (typep obj (type l))
      (call-next-method)
      (error "~A is not of type ~A" obj (type l))))
(defmethod index ((l list) obj &key test)
  (declare (ignore l obj test))
  (error "LIST does not implement INDEX"))

;(defgeneric slice (list i n)
(defgeneric slice (list i &optional n)
  (:documentation "Return the n-element sublist of the list starting at index i. The index may be negative, however, if the index points beyond the beginning of the list an empty sublist is returned."))
;; (defmethod slice :around ((l list) (i integer) (n integer))
;;   (cond ((< n 0) (error "Count N must be non-negative: ~D" n))
;;         ((minusp i)
;;          (let ((j (+ i (size l))))
;;            (if (minusp j)
;;                (slice l 0 0)
;;                (slice l j n))))
;;         (t (call-next-method))))
(defmethod slice :around ((l list) (i integer) &optional n)
  (cond ((minusp i)
         (let ((j (+ i (size l))))
           (if (minusp j)
               (slice l 0 0)
               (slice l j n))))
        ((null n) (call-next-method l i (- (size l) i)))
        ((< n 0) (error "Count N must be non-negative: ~D" n))
        (t (call-next-method))))
;(defmethod slice ((l list) (i integer) (n integer))
(defmethod slice ((l list) (i integer) &optional n)
  (declare (ignore l i n))
  (error "LIST does not implement SLICE"))

;;;
;;;    MUTABLE-LIST
;;;    - SETF NTH can change length of list, but that will increment COUNT-MODIFICATION via EXTEND-LIST -> ADD
;;;
(defclass mutable-list (mutable-collection list)
  ()
  (:documentation "A list whose structure and elements can be modified."))

(defmethod clear :after ((l mutable-list))
  (count-modification l))           

(defmethod add :around ((l mutable-list) &rest objs)
  (declare (ignore l))
  (if (null objs)
      l
      (call-next-method)))
(defmethod add :after ((l mutable-list) &rest objs)
  (declare (ignore objs))
  (count-modification l))

(defmethod insert :after ((l mutable-list) (i integer) obj)
  (declare (ignore i obj))
  (count-modification l))

(defmethod delete :after ((l mutable-list) (i integer))
  (declare (ignore i))
  (count-modification l))


;;;
;;;    This class was designed to allow a distinction between mutable and immutable linked lists,
;;;    analogously to LIST/MUTABLE-LIST. However, these operations that operate on nodes of a
;;;    linked list only make sense for mutable lists. Persistent lists have to rebuild structure,
;;;    so there is no efficiency gain to providing these methods (i.e., referencing a numeric index
;;;    works as well as a pointer to a node.)
;;;    In reality, these methods are only used by list iterators. It should not be possible to
;;;    get ahold of list nodes otherwise. A mutable list can do surgery in place whereas a persistent
;;;    list has to rebuild the head of the list.
;;;    
;; ;;;
;; ;;;    LINKED-LIST
;; ;;;
;; (defclass linked-list (list)
;;   ()
;;   (:documentation "A list implemented with linked nodes."))

;;;
;;;    MUTABLE-LINKED-LIST
;;;
(defclass mutable-linked-list (mutable-list)
  ()
  (:documentation "A list implemented with linked nodes whose structure and elements can be modified."))

;;;    These methods are dangerous? We can't verify that the given NODE is actually
;;;    part of the list structure? (Without expensive linear search which defeats the
;;;    whole point of these methods...)
;;;
;;;    Don't export names. DELETE (public) renamed to DELETE-NODE/DELETE-CHILD
;;;    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Structural modification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric insert-before (list node obj)
  (:documentation "Insert the object before the specified node."))
(defmethod insert-before :around ((l mutable-linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((emptyp l) (error "List is empty")) ; NODE can't belong to L!
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-before ((l mutable-linked-list) node obj)
  (declare (ignore l node obj))
  (error "MUTABLE-LINKED-LIST does not implement INSERT-BEFORE"))
(defmethod insert-before :after ((l mutable-linked-list) node obj)
  (declare (ignore node obj))
  (count-modification l))

(defgeneric insert-after (list node obj)
  (:documentation "Insert the object after the specified node."))
(defmethod insert-after :around ((l mutable-linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((emptyp l) (error "List is empty")) ; NODE can't belong to L!
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-after ((l mutable-linked-list) node obj)
  (declare (ignore l node obj))
  (error "MUTABLE-LINKED-LIST does not implement INSERT-AFTER"))
(defmethod insert-after :after ((l mutable-linked-list) node obj)
  (declare (ignore node obj))
  (count-modification l))

(defgeneric delete-node (list doomed)
  (:documentation "Delete the specified node from the list."))
(defmethod delete-node :around ((l mutable-linked-list) doomed)
  (cond ((emptyp l) (error "List is empty")) ; DOOMED can't belong to L!
        ((null doomed) (error "Invalid node"))
        (t (call-next-method))))
(defmethod delete-node ((l mutable-linked-list) doomed)
  (declare (ignore l doomed))
  (error "MUTABLE-LINKED-LIST does not implement DELETE-NODE"))
(defmethod delete-node :after ((l mutable-linked-list) doomed)
  (declare (ignore doomed))
  (count-modification l))

(defgeneric delete-child (list parent)
  (:documentation "Delete the child of the specified node from the list."))
(defmethod delete-child :around ((l mutable-linked-list) parent)
  (cond ((emptyp l) (error "List is empty")) ; PARENT can't belong to L!
        ((null parent) (error "Invalid node"))
        (t (call-next-method))))
(defmethod delete-child ((l mutable-linked-list) parent)
  (declare (ignore l parent))
  (error "MUTABLE-LINKED-LIST does not implement DELETE-CHILD"))
(defmethod delete-child :after ((l mutable-linked-list) parent)
  (declare (ignore parent))
  (count-modification l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    ARRAY-LIST
;;;    
(defclass array-list (mutable-list)
  ((store)))

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
  (with-slots (store) l
    (setf (fill-pointer store) 0)))

(defun make-random-access-cursor (l)
  (let ((index 0))
    (make-instance 'cursor
                   :done #'(lambda ()
                             (assert (<= index (size l)) () "Index is out of bounds: ~D" index)
                             (= index (size l)))
                   :current #'(lambda () (nth l index))
                   :advance #'(lambda () (incf index)))) )

(defmethod iterator ((l array-list))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-random-access-cursor l))))

(defmethod list-iterator ((l array-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (modification-count) l
                                   ((modification-count #'(lambda () modification-count)))) ))

(defmethod contains ((l array-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

(defmethod add ((l array-list) &rest objs)
  (with-slots (store) l
    (dolist (obj objs l) ; Allocate new array?
      (vector-push-extend obj store))))

;;;
;;;    i < -size => error or no effect?
;;;    Add elt to end: (insert al (size al) x)
;;;    Can't do this with negative index
;;;    
(defmethod insert ((l array-list) (i integer) obj)
  (with-slots (store) l
    (vector-push-extend (fill-elt l) store)
    (setf (subseq store (1+ i)) (subseq store i)
;          (aref store i) obj)))
          (nth l i) obj)))

(defmethod delete ((l array-list) (i integer))
  (with-slots (store) l
;    (prog1 (aref store i)
    (prog1 (nth l i)
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

;; (defmethod slice ((l array-list) (i integer) (n integer))
;;   (with-slots (store) l
;;     (let ((al (make-array-list :type (type l) :fill-elt (fill-elt l)))
;;           (count (size l)))
;; ;      (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (aref store k)))
;;       (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (nth l k)))
;;       al)))

(defmethod slice ((l array-list) (i integer) &optional n)
  (let ((al (make-array-list :type (type l) :fill-elt (fill-elt l)))
        (count (size l)))
    (apply #'add al (loop for j from (min i count) below (min (+ i n) count) collect (nth l j)))) )

;;;
;;;    ARRAY-LIST-X
;;;    - Add OFFSET slot:
;;;      DELETE - Move the smaller portion of the array to close the gap
;;;      INSERT - Reuse space at front of array if possible.
;;;    
;;;    - Moving a smaller SUBSEQ does seem to be faster.
;;;    (let ((a (make-array 1000 :adjustable t :initial-contents (loop for i from 1 to 1000 collect i)))) (time (dotimes (i 100000) (setf (subseq a 200) (subseq a 201)))))
;;;    (let ((a (make-array 1000 :adjustable t :initial-contents (loop for i from 1 to 1000 collect i)))) (time (dotimes (i 100000) (setf (subseq a 1) (subseq a 0 200)))))
;;;    
;;;    - Must release deleted elts to allow GC.
;;;    - More complicated indexing!
;;;    
(defclass array-list-x (mutable-list)
  ((store)
   (offset :initform 0 :type integer)))

(defmethod initialize-instance :after ((l array-list-x) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store) l
    (setf store (make-array 20 :adjustable t :fill-pointer 0 :element-type (type l)))) )

(defun make-array-list-x (&key (type t) (fill-elt nil))
 (make-instance 'array-list-x :type type :fill-elt fill-elt))

(defmethod size ((l array-list-x))
  (with-slots (store offset) l
    (- (length store) offset)))

(defmethod emptyp ((l array-list-x))
  (zerop (size l)))

(defmethod clear ((l array-list-x))
  (with-slots (store offset) l
    (setf (fill-pointer store) 0
          offset 0)))

(defmethod iterator ((l array-list-x))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-random-access-cursor l))))

(defmethod list-iterator ((l array-list-x) &optional (start 0))
  (make-instance 'random-access-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (modification-count) l
                                   ((modification-count #'(lambda () modification-count)))) ))

(defmethod contains ((l array-list-x) obj &key (test #'eql))
  (with-slots (store offset) l
    (find obj store :start offset :test test)))

(defmethod add ((l array-list-x) &rest objs)
  (with-slots (store) l
    (dolist (obj objs l)
      (vector-push-extend obj store))))

(defmethod insert ((l array-list-x) (i integer) obj)
  (with-slots (store offset fill-elt) l
    (let ((j (+ i offset)))
      (cond ((or (zerop offset)
                 (> i (floor (size l) 2)))
             (vector-push-extend fill-elt store)
             (setf (subseq store (1+ j)) (subseq store j)))
            (t (decf offset)
               (setf (subseq store offset) (subseq store (1+ offset) j))))
;      (setf (aref store (+ i offset)) obj))))
      (setf (nth l i) obj))))

(defmethod delete ((l array-list-x) (i integer))
  (with-slots (store offset fill-elt) l
    (let ((j (+ i offset)))
;      (prog1 (aref store j)
      (prog1 (nth l i)
        (cond ((= i 0) ; This doesn't make a bit of difference?!   Cuts 30%?!?!
;               (setf (aref store offset) fill-elt)
               (setf (nth l 0) fill-elt)
               (incf offset))
	      ((<= i (floor (size l) 2)) ; j => 100X time ?!?
               (setf (subseq store (1+ offset)) (subseq store offset j)
;                     (aref store offset) fill-elt)
                     (nth l 0) fill-elt)
               (incf offset))
              (t (setf (subseq store j) (subseq store (1+ j)))
                 (vector-pop store)))) )))

(defmethod nth ((l array-list-x) (i integer))
  (with-slots (store offset) l
    (aref store (+ i offset))))

(defmethod (setf nth) (obj (l array-list-x) (i integer))
  (with-slots (store offset) l
    (setf (aref store (+ i offset)) obj)))

(defmethod index ((l array-list-x) obj &key (test #'eql))
  (with-slots (store offset) l
    (let ((pos (position obj store :start offset :test test)))
      (if (null pos)
          pos
          (- pos offset)))) )

;; (defmethod slice ((l array-list-x) (i integer) (n integer))
;;   (with-slots (store offset) l
;;     (let ((al (make-array-list-x :type (type l) :fill-elt (fill-elt l)))
;;           (count (size l)))
;; ;      (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (aref store (+ k offset))))
;;       (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (nth l k)))
;;       al)))

(defmethod slice ((l array-list-x) (i integer) &optional n)
  (let ((al (make-array-list-x :type (type l) :fill-elt (fill-elt l)))
        (count (size l)))
    (apply #'add al (loop for j from (min i count) below (min (+ i n) count) collect (nth l j)))) )

;;;
;;;    SINGLY-LINKED-LIST
;;;    
(defclass singly-linked-list (mutable-linked-list)
  ((store :initform '())
   (count :initform 0)))

(defun make-linked-list (&key (type t) (fill-elt nil))
  (make-instance 'singly-linked-list :type type :fill-elt fill-elt))

(defmethod size ((l singly-linked-list))
  (slot-value l 'count))

(defmethod emptyp ((l singly-linked-list))
  (with-slots (store) l
    (null store)))

(defmethod clear ((l singly-linked-list))
  (with-slots (store count) l
    (setf store '()
          count 0)))

(defun make-singly-linked-list-cursor (node)
  (make-instance 'cursor
                 :done #'(lambda () (null node))
                 :current #'(lambda () (first node))
                 :advance #'(lambda () (cl:pop node))))
  
(defmethod iterator ((l singly-linked-list))
  (with-slots (modification-count store) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-singly-linked-list-cursor store))))

(defmethod list-iterator ((l singly-linked-list) &optional (start 0))
  (make-instance 'singly-linked-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (store modification-count) l
                                   ((modification-count #'(lambda () modification-count))
                                    (head-node #'(lambda () store)))) ))

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
  (with-slots (store count) l
    (loop for i from 0
          for elt in objs
          collect elt into elts
          finally (progn (setf store (nconc store elts))
                         (incf count i))))
  l)

;;;
;;;    This is substantially the same as INSERT-BEFORE, but they must remain distinct to prevent
;;;    superclass :AFTER methods from incrementing MODIFICATION-COUNT twice!
;;;    
(defmethod insert ((l singly-linked-list) (i integer) obj)
  (with-slots (store) l
    (splice-before (nthcdr i store) obj)))
(defmethod insert :after ((l singly-linked-list) (i integer) obj)
  (declare (ignore i obj))
  (with-slots (count) l
    (incf count)))

;;;
;;;    Is NODE actually part of the structure of L???
;;;    - If not, MODIFICATION-COUNT, COUNT are modified inappropriately.
;;;    
(defmethod insert-before ((l singly-linked-list) node obj)
  (declare (ignore l))
  (splice-before node obj))
(defmethod insert-before :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defmethod insert-after ((l singly-linked-list) node obj)
  (declare (ignore l))
  (splice-after node obj))
(defmethod insert-after :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defmethod delete ((l singly-linked-list) (i integer))
  (with-slots (store) l
    (cond ((zerop i)
           (prog1 (first store)
             (setf store (rest store))))
          (t (excise-child (nthcdr (1- i) store)))) ))
(defmethod delete :after ((l singly-linked-list) (i integer))
  (declare (ignore i))
  (with-slots (count) l
    (decf count)))

;;;    Doesn't need :AROUND method? REMOVE has :AROUND method...
;;;    No way to confirm that NODE is actually part of list? Doesn't matter here? See DLL
;;;

;;;    DELETE-NODE cannot delete last node in list! (Unless node is sole element.)
;;;    - NODE is target. Copy child and route around it.
;;;    (Child is actually removed.)
(defmethod delete-node ((l singly-linked-list) (doomed cons))
  (with-slots (store) l
    (cond ((eq doomed store) ; First elt
	   (prog1 (first store)
	     (setf store (rest store))))
	  (t (excise-node doomed)))) )
(defmethod delete-node :after ((l singly-linked-list) (doomed cons))
  (declare (ignore doomed))
  (with-slots (count) l
    (decf count)))

;;;
;;;    The refactoring below (similar to DOUBLY-LINKED-LIST) looks nice, but
;;;    it doesn't work. DELETE and DELETE-NODE are not interchangeable. In
;;;    particular, DELETE must call EXCISE-CHILD to remove last node of list
;;;    for SINGLY-LINKED-LIST! DELETE-NODE and (EXCISE-NODE) cannot handle the
;;;    last node.
;;;    
;; (defmethod delete ((l singly-linked-list) (i integer))
;;   (with-slots (store) l
;;     (delete-cons l (nthcdr i store))))
;; (defmethod delete :after ((l singly-linked-list) (i integer))
;;   (declare (ignore i))
;;   (with-slots (count) l
;;     (decf count)))

;; ;;;    Doesn't need :AROUND method? REMOVE has :AROUND method...
;; ;;;    No way to confirm that NODE is actually part of list? Doesn't matter here? See DLL
;; ;;;

;; ;;;    DELETE-NODE cannot delete last node in list! (Unless node is sole element.)
;; ;;;    - NODE is target. Copy child and route around it.
;; ;;;    (Child is actually removed.)
;; (defmethod delete-node ((l singly-linked-list) (doomed cons))
;;   (delete-cons l doomed))
;; (defmethod delete-node :after ((l singly-linked-list) (doomed cons))
;;   (declare (ignore doomed))
;;   (with-slots (count) l
;;     (decf count)))

;; (defun delete-cons (l doomed)
;;   (with-slots (store) l
;;     (cond ((eq doomed store) ; First elt
;; 	   (prog1 (first store)
;; 	     (setf store (rest store))))
;; 	  (t (excise-node doomed)))) )
  
;;;    DELETE-CHILD cannot delete 1st node in list!
;;;    - Route around child.
;;;    - PARENT cannot itself be last elt.
(defmethod delete-child ((l singly-linked-list) (parent cons))
  (declare (ignore l))
  (excise-child parent))
(defmethod delete-child :after ((l singly-linked-list) (parent cons))
  (declare (ignore parent))
  (with-slots (count) l
    (decf count)))

(defmethod nth ((l singly-linked-list) (i integer))
  (with-slots (store) l
    (cl:nth i store)))

(defmethod (setf nth) (obj (l singly-linked-list) (i integer))
  (with-slots (store) l
    (setf (cl:nth i store) obj)))

(defmethod index ((l singly-linked-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

;; (defmethod slice ((l singly-linked-list) (i integer) (n integer))
;;   (with-slots (store count) l
;;     (let ((sll (make-linked-list :type (type l) :fill-elt (fill-elt l))))
;;       (apply #'add sll (subseq store (min i count) (min (+ i n) count)))
;;       sll)))

(defmethod slice ((l singly-linked-list) (i integer) &optional n)
  (with-slots (store count) l
    (let ((sll (make-linked-list :type (type l) :fill-elt (fill-elt l))))
      (apply #'add sll (subseq store (min i count) (min (+ i n) count)))) ))

;;;
;;;    SINGLY-LINKED-LIST-X (TCONC)
;;;    - Keeping track of the rear makes ADD _much_ faster!
;;;    
(defclass singly-linked-list-x (mutable-linked-list)
  ((front :initform nil)
   (rear :initform nil)
   (count :initform 0)))

;;;
;;;    Is this a reasonable way to enforce this?
;;;    Whenever FRONT is nil, ensure that REAR is too...
;;;    
;; (defmethod (setf front) :after (obj (l singly-linked-list-x)) ; ???????????????????????
;;   (declare (ignore obj))
;;   (when (null (front l))
;;     (setf (rear l) nil)))

;; (defmethod (setf store) :after (obj (l singly-linked-list-x)) ; ???????????????????????
;;   (declare (ignore obj))
;;   (when (null (front l))
;;     (setf (rear l) nil)))

(defun make-linked-list-x (&key (type t) (fill-elt nil))
  (make-instance 'singly-linked-list-x :type type :fill-elt fill-elt))

(defmethod size ((l singly-linked-list-x))
  (slot-value l 'count))

(defmethod emptyp ((l singly-linked-list-x))
  (with-slots (front) l
    (null front)))

(defmethod clear ((l singly-linked-list-x))
  (with-slots (front rear count) l
    (setf front nil
          rear nil
          count 0)))

(defmethod iterator ((l singly-linked-list-x))
  (with-slots (modification-count front) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-singly-linked-list-cursor front))))

(defmethod list-iterator ((l singly-linked-list-x) &optional (start 0))
  (make-instance 'singly-linked-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (front modification-count) l
                                   ((modification-count #'(lambda () modification-count))
                                    (head-node #'(lambda () front)))) ))

(defmethod contains ((l singly-linked-list-x) obj &key (test #'eql))
  (with-slots (front) l
    (find obj front :test test)))

;;;
;;;    Copy OBJS list and find its length in one traversal.
;;;    
(defmethod add ((l singly-linked-list-x) &rest objs)
  (with-slots (front rear count) l
    (labels ((add-nodes (objs)
               (loop for i from 0
                     for obj in objs
                     do (setf rear (setf (rest rear) (cl:list obj))) ; Enqueue
                     finally (incf count i))))
      (cond ((emptyp l)
             (setf rear (setf front (cl:list (first objs))))
             (incf count)
             (add-nodes (rest objs)))
            (t (add-nodes objs)))) )
  l)

;;;
;;;    This is substantially the same as INSERT-BEFORE, but they must remain distinct to prevent
;;;    superclass :AFTER methods from incrementing MODIFICATION-COUNT twice!
;;;    
(defmethod insert ((l singly-linked-list-x) (i integer) obj)
  (with-slots (front rear) l
    (let ((node (nthcdr i front)))
      (splice-before node obj)
      (when (eq node rear)
	(setf rear (rest rear)))) ))
(defmethod insert :after ((l singly-linked-list-x) (i integer) obj)
  (declare (ignore i obj))
  (with-slots (count) l
    (incf count)))

(defmethod insert-before ((l singly-linked-list-x) node obj)
  (with-slots (rear) l
    (splice-before node obj)
    (when (eq node rear)
      (setf rear (rest rear)))) )
(defmethod insert-before :after ((l singly-linked-list-x) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defmethod insert-after ((l singly-linked-list-x) node obj)
  (with-slots (rear) l
    (splice-after node obj)
    (when (eq node rear)
      (setf rear (rest rear)))) )
(defmethod insert-after :after ((l singly-linked-list-x) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defmethod delete ((l singly-linked-list-x) (i integer))
  (with-slots (front rear) l
    (cond ((zerop i)
           (prog1 (first front)
             (setf front (rest front))
             (when (null front)
               (setf rear nil))))
          (t (let ((parent (nthcdr (1- i) front)))
	       (prog1 (excise-child parent)
		 (when (null (rest parent))
		   (setf rear parent)))) ))))
(defmethod delete :after ((l singly-linked-list-x) (i integer))
  (declare (ignore i))
  (with-slots (count) l
    (decf count)))

;;;    Doesn't need :AROUND method? REMOVE has :AROUND method...
;;;    No way to confirm that NODE is actually part of list? Doesn't matter here? See DLL
;;;

;;;    DELETE-NODE cannot delete last node in list! (Unless node is sole element.)
;;;    - NODE is target. Copy child and route around it.
;;;    (Child is actually removed.)
(defmethod delete-node ((l singly-linked-list-x) (doomed cons))
  (with-slots (front rear) l
    (cond ((eq doomed front)
           (prog1 (first front)
	     (setf front (rest front))
             (when (null front)
               (setf rear nil))))
	  (t (prog1 (excise-node doomed)
               (when (null (rest doomed))
                 (setf rear doomed)))) )))
(defmethod delete-node :after ((l singly-linked-list-x) (doomed cons))
  (declare (ignore doomed))
  (with-slots (count) l
    (decf count)))

;;;    DELETE-CHILD cannot delete 1st node in list!
;;;    - Route around child.
;;;    - PARENT cannot itself be last elt.
(defmethod delete-child ((l singly-linked-list-x) (parent cons))
  (with-slots (rear) l
    (prog1 (excise-child parent)
      (when (null (rest parent))
        (setf rear parent)))) )
(defmethod delete-child :after ((l singly-linked-list-x) (parent cons))
  (declare (ignore parent))
  (with-slots (count) l
    (decf count)))

(defmethod nth ((l singly-linked-list-x) (i integer))
  (with-slots (front) l
    (cl:nth i front)))

(defmethod (setf nth) (obj (l singly-linked-list-x) (i integer))
  (with-slots (front) l
    (setf (cl:nth i front) obj)))

(defmethod index ((l singly-linked-list-x) obj &key (test #'eql))
  (with-slots (front) l
    (position obj front :test test)))

;; (defmethod slice ((l singly-linked-list-x) (i integer) (n integer))
;;   (with-slots (front count) l
;;     (let ((sllx (make-linked-list-x :type (type l) :fill-elt (fill-elt l))))
;;       (apply #'add sllx (subseq front (min i count) (min (+ i n) count)))
;;       sllx)))

(defmethod slice ((l singly-linked-list-x) (i integer) &optional n)
  (with-slots (front count) l
    (let ((sllx (make-linked-list-x :type (type l) :fill-elt (fill-elt l))))
      (apply #'add sllx (subseq front (min i count) (min (+ i n) count)))) ))

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
  ((previous :accessor previous :initarg :previous :initform nil :type (or null dcons))
   (content :accessor content :initarg :content :initform nil)
   (next :accessor next :initarg :next :initform nil :type (or null dcons))))

(defmethod print-object ((dcons dcons) stream)
  (print-unreadable-object (dcons stream)
    (print-previous stream dcons)
    (format stream "~S" (content dcons))
    (print-next stream dcons)))

(defun print-previous (stream dcons)
  (cond ((null (previous dcons)) (format stream "∅ ← "))
        ((eq dcons (previous dcons)) (format stream "↻ "))
        (t (format stream "~S ← " (content (previous dcons)))) ))

(defun print-next (stream dcons)
  (cond ((null (next dcons)) (format stream " → ∅"))
        ((eq dcons (next dcons)) (format stream " ↺"))
        (t (format stream " → ~S" (content (next dcons)))) ))

;;;
;;;    Toyed with calling this function DCONS. Not quite analogous with CONS...
;;;    It doesn't create a DCONS object but instead modifies the two passed in.
;;;    
(defun dlink (previous next)
  (setf (next previous) next
        (previous next) previous))

(defmethod splice-before ((node dcons) obj)
  (let ((new-dcons (make-instance 'dcons :content obj)))
    (dlink (previous node) new-dcons)
    (dlink new-dcons node)))
  
(defmethod splice-after ((node dcons) obj)
  (let ((new-dcons (make-instance 'dcons :content obj)))
    (dlink new-dcons (next node)) ; Do this in the right order!!
    (dlink node new-dcons)))

(defmethod excise-node ((doomed dcons))
  (cond ((eq doomed (next doomed)) (error "Cannot delete sole node."))
        (t (prog1 (content doomed)
             (dlink (previous doomed) (next doomed)))) ))

(defmethod excise-child ((parent dcons))
  (let ((child (next parent)))
    (if (eq parent child)
        (error "Parent must have child node")
        (prog1 (content child)
          (dlink parent (next child)))) ))

;;;
;;;    Alternative DCONS representation
;;;    
;; (defclass dcons ()
;;   ((node)))

;; (defmethod initialize-instance :after ((dcons dcons) &rest initargs &key content)
;;   (declare (ignore initargs))
;;   (with-slots (node) dcons
;;     (setf node (cons content (cons nil nil)))) )

;; (defmethod content ((dcons dcons))
;;   (with-slots (node) dcons
;;     (first node)))
;; (defmethod (setf content) (obj (dcons dcons))
;;   (with-slots (node) dcons
;;     (setf (first node) obj)))
;; (defmethod previous ((dcons dcons))
;;   (with-slots (node) dcons
;;     (first (rest node))))
;; (defmethod (setf previous) (obj (dcons dcons))
;;   (with-slots (node) dcons
;;     (setf (first (rest node)) obj)))
;; (defmethod next ((dcons dcons))
;;   (with-slots (node) dcons
;;     (rest (rest node))))
;; (defmethod (setf next) (obj (dcons dcons))
;;   (with-slots (node) dcons
;;     (setf (rest (rest node)) obj)))

;;;
;;;    Fully encapsulated state. Only accessible by closures.
;;;    
(defclass private-dcons ()
  ((content)
   (set-content)
   (previous)
   (set-previous)
   (next)
   (set-next)))

(defmethod initialize-instance :after ((dcons private-dcons) &rest initargs &key ((:content c)))
  (declare (ignore initargs))
  (let ((n nil)
        (p nil))
    (with-slots (content set-content previous set-previous next set-next) dcons
      (setf content #'(lambda () c)
            set-content #'(lambda (obj) (setf c obj))
            previous #'(lambda () p)
            set-previous #'(lambda (node) (setf p node))
            next #'(lambda () n)
            set-next #'(lambda (node) (setf n node)))) ))

;;;
;;;    There is an intimate connection between a cursor and its associated list.
;;;    Cursor will be attached to a list at the list's creation. However, the list
;;;    will be empty at that point, so there is no meaningful node to attach the
;;;    cursor to.
;;;    
;;;    [Always RESET cursor when modifying list (ADD/INSERT/DELETE)]
;;;    - Initializes first node  <- Only ADD/INSERT
;;;    - Index is potentially invalid anyway <- Any time NTH-DCONS modifies CURSOR!
;;;
(defclass dcursor ()
  ((node :type (or null dcons))
   (index :initform 0)
   (remote-control :initarg :remote-control))
  (:documentation "Cursor for circular doubly-linked list."))

(defmethod initialize-instance :after ((c dcursor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (node remote-control) c
    (setf node (press remote-control head-node)))) ; Empty LIST => (null node)

(defun initializedp (cursor)
  (not (null (slot-value cursor 'node))))

(defun reset (cursor)
  (with-slots (node index remote-control) cursor
    (setf node (press remote-control head-node)
          index 0)))

(defun at-start-p (cursor)
  (or (not (initializedp cursor))
      (zerop (slot-value cursor 'index))))

(defun at-end-p (cursor)
  (with-slots (index remote-control) cursor
    (or (not (initializedp cursor))
        (= index (1- (press remote-control size)))) ))

(defgeneric advance (cursor &optional step)
  (:documentation "Advance the cursor to the next node or ahead multiple nodes."))
(defmethod advance :around ((c dcursor) &optional step)
  (declare (ignore step))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod advance ((c dcursor) &optional (step 1))
  (assert (plusp step) () "STEP must be a positive value: ~A" step)
  (with-slots (node index remote-control) c
    (loop repeat step
          do (incf index)
             (setf node (next node)))
    (setf index (mod index (press remote-control size)))) )

(defgeneric rewind (cursor &optional step)
  (:documentation "Rewind the cursor to the previous node or back multiple nodes."))
(defmethod rewind :around ((c dcursor) &optional step)
  (declare (ignore step))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod rewind ((c dcursor) &optional (step 1))
  (assert (plusp step) () "STEP must be a positive value: ~A" step)
  (with-slots (node index remote-control) c
    (loop repeat step
          do (decf index)
             (setf node (previous node)))
    (setf index (mod index (press remote-control size)))) )

;;;
;;;    These can allow DCURSOR to get out of sync!
;;;    
(defgeneric bump (cursor)
  (:documentation "Bump cursor ahead by one node without updating index."))
(defmethod bump :around ((c dcursor))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod bump ((c dcursor))
  (with-slots (node) c
    (setf node (next node))))

(defgeneric nudge (cursor)
  (:documentation "Nudge index ahead by one without adjusting node."))
(defmethod nudge :around ((c dcursor))
  (if (initializedp c)
      (call-next-method)
      (error "Cursor has not been initialized")))
(defmethod nudge ((c dcursor))
  (with-slots (index) c
    (incf index)))

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
  (make-instance 'doubly-linked-list-iterator :collection l))

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
          (add-nodes store dcons (rest objs)))) )
    l))
(defmethod add :after ((l doubly-linked-list-old) &rest objs)
  (unless (null objs)
    (with-slots (cursor) l
      (with-slots (node) cursor
	(when (null node) ; Initialize cursor
          (reset cursor)))) ))

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

(defmethod delete ((l doubly-linked-list-old) (i integer))
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
(defmethod delete :after ((l doubly-linked-list-old) (i integer))
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

;; (defmethod slice ((l doubly-linked-list-old) (i integer) (n integer))
;;   (labels ((dsubseq (start end)
;;              (loop for dcons = (nth-dcons-old l start) then (next dcons)
;;                    for i from start below end
;;                    collect (content dcons))))
;;     (with-slots (type count fill-elt) l
;;       (let ((dll (make-doubly-linked-list-old :type type :fill-elt fill-elt)))
;;         (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))
;;         dll))))

(defmethod slice ((l doubly-linked-list-old) (i integer) &optional n)
  (labels ((dsubseq (start end)
             (loop for dcons = (nth-dcons-old l start) then (next dcons)
                   for i from start below end
                   collect (content dcons))))
    (with-slots (type count fill-elt) l
      (let ((dll (make-doubly-linked-list-old :type type :fill-elt fill-elt)))
        (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    Take two. Only one cursor.
;;;    - Can't make doubly-linked tree? CONTENT of DCONS is singly-linked.
;;;    
(defclass doubly-linked-list (mutable-linked-list)
  ((store :initform nil)
   (count :initform 0)
   (cursor :documentation "Floating cursor. May simplify access based on previous access."))
  (:documentation "Circular doubly-linked list."))

(flet ((setup-cursor (dll)
         (make-instance 'dcursor
                        :remote-control (with-remote (store count) dll
                                          ((head-node #'(lambda () store))
                                           (size #'(lambda () count)))) )))
  (defmethod initialize-instance :after ((l doubly-linked-list) &rest initargs)
    (declare (ignore initargs))
    (with-slots (cursor) l
      (setf cursor (setup-cursor l))))
  (defun make-doubly-linked-list-cursor (l)
    (let ((dcursor (setup-cursor l))
          (sealed-for-your-protection t))
      (make-instance 'cursor
                     :done #'(lambda ()
                               (or (not (initializedp dcursor)) ; ?? Empty list??
                                   (and (not sealed-for-your-protection) (at-start-p dcursor))))
                     :current #'(lambda ()
                                  (with-slots (node) dcursor ; ???
                                    (content node)))
                     :advance #'(lambda ()
                                  (advance dcursor)
                                  (setf sealed-for-your-protection nil)))) )
  (defmethod list-iterator ((l doubly-linked-list) &optional (start 0))
    (make-instance 'doubly-linked-list-list-iterator
                   :list l
                   :start start
                   :remote-control (with-remote (modification-count) l
                                     ((modification-count #'(lambda () modification-count))
                                      (initialize #'(lambda () (setup-cursor l)))) ))))

(defmethod iterator ((l doubly-linked-list))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-doubly-linked-list-cursor l))))

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
  (with-slots (store count) l
    (labels ((add-nodes (start elts)
               (loop for dcons = start then (next dcons)
                     for i from 1
                     for elt in elts
                     do (dlink dcons (make-instance 'dcons :content elt))
                     finally (progn (dlink dcons store)
                                    (incf count i)))) )
      (let ((dcons (make-instance 'dcons :content (first objs))))
        (cond ((emptyp l) (setf store dcons))
              (t (dlink (previous store) dcons)))
        (add-nodes dcons (rest objs)))) )
  l)
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
;; (flet ((nth-dcons (list i)
;;          (declare (integer i))
;;          (with-slots (store count cursor) list
;;            (declare (integer count))
;; ;    (assert (typep i `(integer 0 (,count))) () "Invalid index: ~D" i)
;;            (assert (and (>= i 0) (< i count)) () "Invalid index: ~D" i)
;;            (if (emptyp list)
;;                (error "List is empty")
;;                (with-slots (index node) cursor
;;                  (declare (integer index))
;;                  (cond ((zerop i) store)
;;                        ((= i index) node)
;;                        ((< i (/ index 2)) ; I.e., i - 0 < index - i
;;                         (reset cursor)
;;                         (advance cursor i)
;;                         node)
;;                        ((< i index)
;;                         (rewind cursor (- index i))
;;                         node)
;;                        ;;((<= i (/ (+ count index) 2))
;;                        ((<= (- i index) (- count i))
;;                         (advance cursor (- i index))
;;                         node)
;;                        (t (reset cursor)
;;                           (rewind cursor (- count i))
;;                           node)))) )))
;; (flet ((nth-dcons (list i)
;;          (declare (integer i))
;;          (with-slots (store count cursor) list
;;            (declare (integer count))
;;            (assert (and (>= i 0) (< i count)) () "Invalid index: ~D" i)
;;            (if (emptyp list)
;;                (error "List is empty")
;;                (with-slots (index node) cursor
;;                  (declare (integer index))
;;                  (cond ((zerop i) store)
;;                        ((= i index) node)
;;                        ((< i index)
;;                         (let ((index-delta (- index i)))
;;                           (cond ((< i index-delta) ; I.e., i - 0 < index - i
;;                                  (reset cursor)
;;                                  (advance cursor i)
;;                                  node)
;;                                 (t (rewind cursor index-delta)
;;                                    node))))
;;                        (t (let ((index-delta (- i index))
;;                                 (count-delta (- count i)))
;;                             (cond ((<= index-delta count-delta)
;;                                    (advance cursor index-delta)
;;                                    node)
;;                                   (t (reset cursor)
;;                                      (rewind cursor count-delta)
;;                                      node)))) )))) ))
(labels ((nth-dcons (list i)
           (declare (integer i))
           (if (emptyp list)
               (error "List is empty")
               (with-slots (count cursor) list
                 (declare (integer count))
                 (assert (and (>= i 0) (< i count)) () "Invalid index: ~D" i)
                 (with-slots (node) cursor ; ????
                   (reposition-cursor cursor i count)
                   node))))
         (reposition-cursor (cursor i count)
           (with-slots (index) cursor ; ???
             (declare (integer index))
             (cond ((zerop i) (reset cursor))
                   ((< i index)
                    (let ((index-delta (- index i)))
                      (cond ((< i index-delta) ; I.e., i - 0 < index - i
                             (reset cursor)
                             (advance cursor i))
                            (t (rewind cursor index-delta)))) )
                   ((> i index)
                    (let ((index-delta (- i index))
                          (count-delta (- count i)))
                      (cond ((<= index-delta count-delta)
                             (advance cursor index-delta))
                            (t (reset cursor)
                               (rewind cursor count-delta)))) )))) )
  (defmethod insert ((l doubly-linked-list) (i integer) obj)
    (with-slots (store) l
      (splice-before (nth-dcons l i) obj)
      (when (zerop i)
        (setf store (previous store)))) )
  (defmethod insert :after ((l doubly-linked-list) (i integer) obj)
    (declare (ignore obj))
    (with-slots (count cursor) l
      (incf count)
      (with-slots (index) cursor ; ???
        (when (or (not (initializedp cursor))
                  (<= 0 i index)
                  (and (minusp i) (<= 0 (+ i count) index)))
          (reset cursor)))) )
  (defmethod delete ((l doubly-linked-list) (i integer))
    (delete-dcons l (nth-dcons l i)))
  (defmethod delete :after ((l doubly-linked-list) (i integer))
    (declare (ignore i))
    (with-slots (count cursor) l
      (decf count)
      (reset cursor)))
  (defmethod nth ((l doubly-linked-list) (i integer))
    (with-slots (store count) l
      (content (nth-dcons l i))))
  (defmethod (setf nth) (obj (l doubly-linked-list) (i integer))
    (with-slots (store) l
      (setf (content (nth-dcons l i)) obj)))
  ;; (defmethod slice ((l doubly-linked-list) (i integer) (n integer))
  ;;   (labels ((dsubseq (start end)
  ;;              (loop for i from start below end
  ;;                    for dcons = (nth-dcons l start) then (next dcons)
  ;;                    collect (content dcons))))
  ;;     (with-slots (count) l
  ;;       (let ((dll (make-doubly-linked-list :type (type l) :fill-elt (fill-elt l))))
  ;;         (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))
  ;;         dll)))) )
  (defmethod slice ((l doubly-linked-list) (i integer) &optional n)
    (labels ((dsubseq (start end)
               (loop for i from start below end
                     for dcons = (nth-dcons l start) then (next dcons)
                     collect (content dcons))))
      (with-slots (count) l
        (let ((dll (make-doubly-linked-list :type (type l) :fill-elt (fill-elt l))))
          (apply #'add dll (dsubseq (min i count) (min (+ i n) count)))) ))))

;;;
;;;    Is NODE actually part of the structure of L???
;;;    - If not, MODIFICATION-COUNT, COUNT, CURSOR are modified inappropriately.
;;;    - In fact the whole list can be broken:
;; * (defvar *dll* (make-doubly-linked-list))
;; * (add *dll* 2 3 4)
;; * (defvar *fake* (make-instance 'dcons :content 99))
;; * (setf (next *fake*) (nth-dcons *dll* 2))
;; * (insert-after *dll* *fake* 22)
;; * *dll*
;; #<DOUBLY-LINKED-LIST (2 2.5 3 4 2)>
;; * (nth-dcons *dll* 2)
;; #<22 ← 3 → 4>
;; * (defvar *li* (list-iterator *dll*))
;; * (current *li*)
;; 2
;; * (next *li*)
;; 2.5
;; * (next *li*)
;; 3
;; * (previous *li*)
;; 22

;;;
;;;    Don't need to worry whether cursor is initialized for
;;;    INSERT-BEFORE/-AFTER. List is not empty and every path
;;;    to place an elt in the list (ADD/INSERT/(SETF NTH)) goes
;;;    through ADD, which resets cursor if necessary.
;;;    
(defmethod insert-before ((l doubly-linked-list) node obj)
  (with-slots (store) l
    (splice-before node obj)
    (when (eq store node)
      (setf store (previous store)))) )
(defmethod insert-before :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count cursor) l
    (incf count)
    (reset cursor)))
;;;
;;;    Position of the LIST's DCURSOR is independent of the NODE located by list iterator's ADD-BEFORE.
;;;    Furthermore, we don't know the index of the NODE. If DCURSOR's index is before NODE, then it
;;;    need not be adjusted. If equal or after, it must be incremented. But we can't tell...
;;;    


(defmethod insert-after ((l doubly-linked-list) node obj)
  (declare (ignore l))
  (splice-after node obj))
(defmethod insert-after :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count cursor) l
    (incf count)
    (reset cursor))) ; Same issue as above with INSERT-BEFORE!!!

;;;
;;;    Confirm that DOOMED is actually a node in this list!
;;;    - Smuggling in some fabricated DCONS could snip out multiple nodes depending
;;;      on what its NEXT and PREVIOUS links pointed to.
;;;      
;;;    This method needs to be absolutely private! Risky to split an object up like this...
;;;    
(defmethod delete-node ((l doubly-linked-list) (doomed dcons))
  (delete-dcons l doomed))
(defmethod delete-node :after ((l doubly-linked-list) (doomed dcons))
  (declare (ignore doomed))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor))) ; NTH-DCONS moves cursor in most cases?

(defun delete-dcons (l doomed)
  (with-slots (store) l
    (cond ((eq doomed (next doomed)) ; Single-elt
           (prog1 (content doomed)
             (setf (next doomed) nil ; Free for GC!
                   store '())))
          (t (prog1 (excise-node doomed)
               (when (eq doomed store) ; First elt otherwise
                 (setf store (next doomed)))) ))))

;;;
;;;    DELETE-CHILD not really necessary for DOUBLY-LINKED-LIST.
;;;    
(defmethod delete-child ((l doubly-linked-list) (parent dcons))
  (with-slots (store) l
    (let ((child (next parent)))
      (if (eq child store)
          (error "Parent must have child node")
          (excise-child parent)))) )
(defmethod delete-child :after ((l doubly-linked-list) (doomed dcons))
  (declare (ignore doomed))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor))) ; NTH-DCONS moves cursor in most cases?

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

;;;
;;;    HASH-TABLE-LIST
;;;    
;;;    Oops. This is not as simple as a stack or queue. Since elements can be inserted or deleted
;;;    the deli counter model falls apart. An insertion or deletion could require that the mapping
;;;    of keys to elements be recomputed--similar to shifting elements in an array.
;;;
;;;    Have to be careful using high-level functions in implementation of other such functions despite
;;;    the elegance of this approach.
;;;    
;;;    This is particularly true of functions that modify structure as each such modification must be
;;;    counted only once. This is a problem with ADD/INSERT calling (SETF NTH) rather than GETHASH.
;;;    This can result in calls to EXTEND-LIST, which triggers a hidden call to ADD.
;;;
;;;    It is a problem with DELETE as well even though the implementation is correct.
;;;    It is, however, slower due to the computation of generic functions.
;;;
;;;    It doesn't matter with functions without side effects such as CONTAINS/INDEX/SLICE.
;;;
(defclass hash-table-list (mutable-list)
  ((store :initform (make-hash-table))))

(defun make-hash-table-list (&key (type t) (fill-elt nil))
  (make-instance 'hash-table-list :type type :fill-elt fill-elt))

(defmethod size ((l hash-table-list))
  (with-slots (store) l
    (hash-table-count store)))

(defmethod emptyp ((l hash-table-list))
  (zerop (size l)))

(defmethod clear ((l hash-table-list))
  (with-slots (store) l
    (clrhash store)))

(defmethod iterator ((l hash-table-list))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-random-access-cursor l))))

(defmethod list-iterator ((l hash-table-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (modification-count) l
                                   ((modification-count #'(lambda () modification-count)))) ))

(defmethod contains ((l hash-table-list) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
      (let ((elt (nth l i)))
        (when (funcall test obj elt)
          (return elt)))) ))

(defmethod add ((l hash-table-list) &rest objs)
  (with-slots (store) l
    (loop for i from (size l)
          for obj in objs
;          do (setf (nth l i) obj))))  ; D'oh!!!!!!!!!!!!!!!!!!!!
          do (setf (gethash i store) obj)))
  l)

(defmethod insert ((l hash-table-list) (i integer) obj)
  (with-slots (store) l
    (loop for j from (size l) above i
;          do (setf (nth l j) (nth l (1- j))))
          do (setf (gethash j store) (gethash (1- j) store)))
;    (setf (nth l i) obj)))
    (setf (gethash i store) obj)))
                   
(defmethod delete ((l hash-table-list) (i integer))
  (with-slots (store) l
    (let ((count (size l)))
      (prog1 (nth l i)
        (loop for j from i below (1- count)
              do (setf (gethash j store) (gethash (1+ j) store)))
        (remhash (1- count) store)))) )

;;;
;;;    This version takes 3X the time of the previous in the TEST-LIST-TIME test
;;;    due to the generic function calls NTH/(SETF NTH)...
;;;    
;; (defmethod delete ((l hash-table-list) (i integer))
;;   (with-slots (store) l
;;     (let ((count (size l)))
;;       (prog1 (nth l i)
;;         (loop for j from i below (1- count)
;;               do (setf (nth l j) (nth l (1+ j))))
;;         (remhash (1- count) store)))) )

(defmethod nth ((l hash-table-list) (i integer))
  (with-slots (store) l
    (gethash i store)))

(defmethod (setf nth) (obj (l hash-table-list) (i integer))
  (with-slots (store) l
    (setf (gethash i store) obj)))

(defmethod index ((l hash-table-list) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
;      (when (funcall test obj (gethash i store))
      (when (funcall test obj (nth l i))
        (return i)))) )

;; (defmethod slice ((l hash-table-list) (i integer) (n integer))
;;   (with-slots (store) l
;;     (let ((htl (make-hash-table-list :type (type l) :fill-elt (fill-elt l)))
;;           (count (size l)))
;; ;      (apply #'add htl (loop for k from (min i count) below (min (+ i n) count) collect (gethash k store)))
;;       (apply #'add htl (loop for k from (min i count) below (min (+ i n) count) collect (nth l k)))
;;       htl)))

(defmethod slice ((l hash-table-list) (i integer) &optional n)
  (let ((htl (make-hash-table-list :type (type l) :fill-elt (fill-elt l)))
        (count (size l)))
    (apply #'add htl (loop for j from (min i count) below (min (+ i n) count) collect (nth l j)))) )

;;;
;;;    HASH-TABLE-LIST-X
;;;    - A second hash-table EROTS is used to keep a reverse map of values to keys in
;;;      order to facilitate CONTAINS and INDEX operations. The normal mapping of indices
;;;      to elements need not be injective, so the reverse map may not be a function.
;;;    - The :TEST keyword value affects how objects are located.
;;;    
;; (defvar *htlx1* (make-hash-table-list-x))
;; (defvar *htlx2* (make-hash-table-list-x :test #'equalp))
;; (defvar *htlx3* (make-hash-table-list-x :test #'equal))

;; (add *htlx1* "FOO" "foo" "Foo")
;; (add *htlx2* "FOO" "foo" "Foo")
;; (add *htlx3* "FOO" "foo" "Foo")

;; (contains *htlx1* "foo") => NIL
;; (contains *htlx2* "foo") => "FOO"
;; (contains *htlx3* "foo") => "foo"

;; (contains *htlx1* "foo" :test #'string=) => "foo"
;; (contains *htlx2* "foo" :test #'string=) => "foo"
;; (contains *htlx3* "foo" :test #'string=) => "foo"

;; (index *htlx1* "foo") => NIL
;; (index *htlx2* "foo") => 0
;; (index *htlx3* "foo") => 1

;; (index *htlx1* "foo" :test #'string-equal) => 0
;; (index *htlx1* "foo" :test #'string=) => 1
;; (index *htlx2* "foo" :test #'string=) => 1
;; (index *htlx3* "foo" :test #'string=) => 1

(defclass hash-table-list-x (mutable-list)
  ((store :initform (make-hash-table)) ; Keys are always integers!
   (erots))) ; Should have more restrictive test!???

(defmethod initialize-instance :after ((l hash-table-list-x) &rest initargs &key (test #'eql))
  (declare (ignore initargs))
  (with-slots (erots) l
    (setf erots (make-hash-table :test test))))

(defun make-hash-table-list-x (&key (type t) (fill-elt nil) (test #'eql))
  (make-instance 'hash-table-list-x :type type :fill-elt fill-elt :test test))

(defmethod size ((l hash-table-list-x))
  (with-slots (store) l
    (hash-table-count store)))

(defmethod emptyp ((l hash-table-list-x))
  (zerop (size l)))

(defmethod clear ((l hash-table-list-x))
  (with-slots (store erots) l
    (clrhash store)
    (clrhash erots)))

(defmethod iterator ((l hash-table-list-x))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-random-access-cursor l))))

(defmethod list-iterator ((l hash-table-list-x) &optional (start 0))
  (make-instance 'random-access-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (modification-count) l
                                   ((modification-count #'(lambda () modification-count)))) ))

(defun compatible-equality-test-p (test1 test2)
  "Is TEST2 at least as specific as TEST1?"
  (member test2 (member test1 (cl:list #'equalp #'equal #'eql #'eq))))

(defmethod contains ((l hash-table-list-x) obj &key (test #'eql))
  (with-slots (store erots) l
    (cond ((compatible-equality-test-p (symbol-function (hash-table-test erots)) test)
           (let ((indexes (gethash obj erots)))
             (if (null indexes)
                 nil
                 (gethash (apply #'min indexes) store))))
          (t (dotimes (i (size l) nil)
               (let ((elt (nth l i)))
                 (when (funcall test obj elt)
                   (return elt)))) ))))

(defun symmetric-add (l i obj)
  (with-slots (store erots) l
    (setf (gethash i store) obj)
    (cl:push i (gethash obj erots))))

(defmethod add ((l hash-table-list-x) &rest objs)
  (loop for i from (size l)
        for obj in objs
        do (symmetric-add l i obj))
  l)

(defun clear-erots-index (erots obj i)
  (let* ((indexes (gethash obj erots))
         (indexes* (cl:remove i indexes)))
    (if (null indexes*)
        (remhash obj erots)
        (setf (gethash obj erots) indexes*))))
        
(defmethod insert ((l hash-table-list-x) (i integer) obj)
  (with-slots (store erots) l
    (loop for j from (size l) above i
          for current = (gethash (1- j) store)
;          for current = (nth l (1- j))
          do (clear-erots-index erots current (1- j))
             (symmetric-add l j current))
    (symmetric-add l i obj)))
                   
(defmethod delete ((l hash-table-list-x) (i integer))
  (with-slots (store erots) l
    (let ((count (size l))
;          (doomed (gethash i store)))
          (doomed (nth l i)))
        (loop for j from i below (1- count)
              for current = (gethash (1+ j) store)
;              for current = (nth l (1+ j))
              do (clear-erots-index erots current (1+ j))
                 (symmetric-add l j current))
        (remhash (1- count) store)
        (clear-erots-index erots doomed i)
        doomed)))

(defmethod nth ((l hash-table-list-x) (i integer))
  (with-slots (store) l
    (gethash i store)))

(defmethod (setf nth) (obj (l hash-table-list-x) (i integer))
  (with-slots (store erots) l
;    (clear-erots-index erots (gethash i store) i)
    (clear-erots-index erots (nth l i) i)
    (symmetric-add l i obj)))

(defmethod index ((l hash-table-list-x) obj &key (test #'eql))
  (with-slots (store erots) l
    (cond ((compatible-equality-test-p (symbol-function (hash-table-test erots)) test)
           (let ((indexes (gethash obj erots)))
             (if (null indexes)
                 nil
                 (apply #'min indexes))))
          (t (dotimes (i (size l) nil)
;               (when (funcall test obj (gethash i store))
               (when (funcall test obj (nth l i))
                 (return i)))) )))

;; (defmethod slice ((l hash-table-list-x) (i integer) (n integer))
;;   (with-slots (store) l
;;     (let ((htlx (make-hash-table-list-x :type (type l) :fill-elt (fill-elt l) :test (hash-table-test store)))
;;           (count (size l)))
;; ;      (apply #'add htlx (loop for k from (min i count) below (min (+ i n) count) collect (gethash k store)))
;;       (apply #'add htlx (loop for k from (min i count) below (min (+ i n) count) collect (nth l k)))
;;       htlx)))

(defmethod slice ((l hash-table-list-x) (i integer) &optional n)
  (with-slots (store) l
    (let ((htlx (make-hash-table-list-x :type (type l) :fill-elt (fill-elt l) :test (hash-table-test store)))
          (count (size l)))
      (apply #'add htlx (loop for j from (min i count) below (min (+ i n) count) collect (nth l j)))) ))

;;;
;;;    HASH-TABLE-LIST-Z
;;;    - Similar to ARRAY-LIST-X w/ OFFSET
;;;
(defclass hash-table-list-z (mutable-list)
  ((store :initform (make-hash-table))
   (offset :initform 0 :type integer)))

(defun make-hash-table-list-z (&key (type t) (fill-elt nil))
  (make-instance 'hash-table-list-z :type type :fill-elt fill-elt))

(defmethod size ((l hash-table-list-z))
  (with-slots (store offset) l
    (hash-table-count store)))

(defmethod emptyp ((l hash-table-list-z))
  (zerop (size l)))

(defmethod clear ((l hash-table-list-z))
  (with-slots (store offset) l
    (clrhash store)
    (setf offset 0)))

(defmethod iterator ((l hash-table-list-z))
  (with-slots (modification-count) l
    (make-instance 'mutable-collection-iterator
                   :modification-count #'(lambda () modification-count)
                   :cursor (make-random-access-cursor l))))

(defmethod list-iterator ((l hash-table-list-z) &optional (start 0))
  (make-instance 'random-access-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (modification-count) l
                                   ((modification-count #'(lambda () modification-count)))) ))

(defmethod contains ((l hash-table-list-z) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
      (let ((elt (nth l i)))
        (when (funcall test obj elt)
          (return elt)))) ))
  ;; (with-slots (store offset) l
  ;;   (loop for i from 0 below (size l)
  ;;         for elt = (nth l i)
  ;;         when (funcall test obj elt)
  ;;         do (return elt)
  ;;         finally (return nil))))

(defmethod add ((l hash-table-list-z) &rest objs)
  (with-slots (store offset) l
    (loop for i from (size l)
          for obj in objs
;          do (setf (nth l i) obj))))
          do (setf (gethash (+ i offset) store) obj)))
  l)

(defmethod insert ((l hash-table-list-z) (i integer) obj)
  (with-slots (store offset) l
    (cond ((> i (floor (size l) 2))
           (loop for j from (size l) above i
;                 do (setf (nth l j) (nth l (1- j)))) )
                 do (setf (gethash (+ j offset) store) (gethash (+ (1- j) offset) store))))
          (t (decf offset)
             (loop for j from 0 below i
;                   do (setf (nth l j) (nth l (1+ j)))) ))
                   do (setf (gethash (+ j offset) store) (gethash (+ (1+ j) offset) store)))) )
;    (setf (nth l i) obj)))
    (setf (gethash (+ i offset) store) obj)))
                   
(defmethod delete ((l hash-table-list-z) (i integer))
  (with-slots (store offset) l
    (prog1 (nth l i)
      (cond ((zerop i)
             (remhash offset store)
             (incf offset))
            ((<= i (floor (size l) 2))
             (loop for j from i above 0
                   do (setf (gethash (+ j offset) store) (gethash (+ (1- j) offset) store)))
;                   do (setf (nth l j) (nth l (1- j))))
             (remhash offset store)
             (incf offset))
            (t (loop for j from i below (1- (size l))
                     do (setf (gethash (+ j offset) store) (gethash (+ (1+ j) offset) store)))
;                     do (setf (nth l j) (nth l (1+ j))))
               (remhash (+ (1- (size l)) offset) store)))) ))

(defmethod nth ((l hash-table-list-z) (i integer))
  (with-slots (store offset) l
    (gethash (+ i offset) store)))

(defmethod (setf nth) (obj (l hash-table-list-z) (i integer))
  (with-slots (store offset) l
    (setf (gethash (+ i offset) store) obj)))

(defmethod index ((l hash-table-list-z) obj &key (test #'eql))
  (with-slots (store offset) l
    (dotimes (i (size l) nil)
;      (when (funcall test obj (gethash (+ i offset) store))
      (when (funcall test obj (nth l i))
        (return i)))) )

;; (defmethod slice ((l hash-table-list-z) (i integer) (n integer))
;;   (with-slots (store offset) l
;;     (let ((htlz (make-hash-table-list-z :type (type l) :fill-elt (fill-elt l)))
;;           (count (size l)))
;; ;      (apply #'add htlz (loop for k from (min i count) below (min (+ i n) count) collect (gethash (+ k offset) store)))
;;       (apply #'add htlz (loop for k from (min i count) below (min (+ i n) count) collect (nth l k)))
;;       htlz)))

(defmethod slice ((l hash-table-list-z) (i integer) &optional n)
  (let ((htlz (make-hash-table-list-z :type (type l) :fill-elt (fill-elt l)))
        (count (size l)))
    (apply #'add htlz (loop for j from (min i count) below (min (+ i n) count) collect (nth l j)))) )

;;;
;;;    PERSISTENT-LIST
;;;    - No point in making this a subclass of LINKED-LIST.
;;;      No advantage to having reference to "current" node since structure must be rebuilt.
;;; 
;; (defclass persistent-list (list)
;;   ((store :initform '() :initarg :store)
;;    (count :initform 0 :type integer)))

(defclass persistent-list (list)
  ((store :initform '())
   (count :initform 0 :type integer)))

;; (defmethod initialize-instance :after ((l persistent-list) &rest initargs)
;;   (declare (ignore initargs))
;;   (with-slots (store count) l
;;     (setf count (length store))))

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

(defmethod equals ((l1 list) (l2 persistent-list) &key (test #'eql))
  (equals l2 l1 :test #'(lambda (x y) (funcall test y x))))
(defmethod equals ((l1 persistent-list) (l2 list) &key (test #'eql))
  (if (= (size l1) (size l2))
      (let ((i2 (iterator l2)))
        (loop for i1 = (iterator l1) then (next i1)
              do (cond ((done i1) (return (done i2)))
                       ((done i2) (return nil))
                       ((funcall test (current i1) (current i2)) (next i2))
                       (t (return nil)))) )
      nil))
(defmethod equals ((l1 persistent-list) (l2 persistent-list) &key (test #'eql))
  (if (= (size l1) (size l2))
      (loop for i1 = (iterator l1) then (next i1)
            for i2 = (iterator l2) then (next i2)
            do (cond ((done i1) (return (done i2)))
                     ((done i2) (return nil))
                     ((not (funcall test (current i1) (current i2))) (return nil))))
      nil))

(defmethod each ((l persistent-list) op)
  (loop for i = (iterator l) then (next i)
        until (done i)
        do (funcall op (current i))))

(defmethod size ((l persistent-list))
  (slot-value l 'count))

(defmethod emptyp ((l persistent-list))
  (null (slot-value l 'store)))

(defmethod clear ((l persistent-list))
  (make-persistent-list :type (type l) :fill-elt (fill-elt l)))

(defun make-persistent-list-cursor (l)
  (make-instance 'cursor
                 :done #'(lambda () (emptyp l))
                 :current #'(lambda () (nth l 0))
                 :advance #'(lambda () (iterator (delete l 0)))) )
                 
(defmethod iterator ((l persistent-list))
  (make-instance 'persistent-collection-iterator
                 :cursor (make-persistent-list-cursor l)))
                 
(defmethod list-iterator ((l persistent-list) &optional (start 0))
  ;;    Does this have to be here??? 见 MAKE-INSTANCE :AROUND
  (assert (typep start `(integer 0 (,(max (size l) 1)))) () "Invalid index: ~D" start)
  (make-instance 'persistent-list-list-iterator
                 :list l
                 :start start
                 :remote-control (with-remote (store) l
                                   ((head-node #'(lambda () store)))) ))

(defmethod contains ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

;;;
;;;    INITIALIZE-LIST is completely private now!
;;;    
(flet ((initialize-list (type fill-elt store count)
         (let ((new-list (make-instance 'persistent-list :type type :fill-elt fill-elt)))
           (with-slots ((new-store store) (new-count count)) new-list
             (setf new-store store
                   new-count count))
           new-list))
       (adjust-node (store i adjustment) ; Creates ad hoc queue
         (do ((front nil)
              (rear nil)
              (node store (rest node))
              (j 0 (1+ j)))
             ((>= j i) (let ((tail (funcall adjustment node)))
                         (if (null front)
                             (setf front tail)
                             (setf (rest rear) tail))
                         (values front node)))
           (let ((copy (cons (first node) nil)))
             (cond ((null front) (setf rear (setf front copy)))
                   (t (setf rear (setf (rest rear) copy)))) ))))
  (defmethod add ((l persistent-list) &rest objs)
    (if (null objs)
        l
        (with-slots (type fill-elt store count) l
          (loop for elt in objs
                for i from 1
                collect elt into elts
                finally (return (initialize-list type fill-elt (append store elts) (+ i count)))) )))
  (defmethod insert ((l persistent-list) (i integer) obj)
    (with-slots (type fill-elt store count) l
      (initialize-list type fill-elt (adjust-node store i #'(lambda (node) (cons obj node))) (1+ count))))
  (defmethod delete :around ((l persistent-list) (i integer))
    (cond ((emptyp l) (error "List is empty")) ; ??????
          ((>= i (size l)) l)
          ((< i (- (size l))) l)
          (t (call-next-method))))
  (defmethod delete ((l persistent-list) (i integer))
    (with-slots (type fill-elt store count) l
      (multiple-value-bind (new-store doomed)
;          (adjust-node store i #'(lambda (node) (rest node)))
          (adjust-node store i #'rest)
        (values (initialize-list type fill-elt new-store (1- count)) (first doomed)))) )
;;;
;;;    SETF method need not actually set anything???
;;;    Simply used for value...
;;;    http://www.lispworks.com/documentation/HyperSpec/Body/05_abi.htm
;;;    
  (defmethod (setf nth) (obj (l persistent-list) (i integer))
    (with-slots (type fill-elt store count) l
      (initialize-list type fill-elt (adjust-node store i #'(lambda (node) (cons obj (rest node)))) count)))
  ;;
  ;;    Why doesn't this use ADD as the other SLICE methods do?
  ;;    Simply for performance? Avoids creating a lot of garbage...
  ;;    
  ;; (defmethod slice ((l persistent-list) (i integer) (n integer))
  ;;   (with-slots (type fill-elt store count) l
  ;;     (let* ((start (min i count))
  ;;            (end (min (+ i n) count)))
  ;;       (initialize-list type fill-elt (subseq store start end) (- end start)))) ))
  (defmethod slice ((l persistent-list) (i integer) &optional n)
    (with-slots (type fill-elt store count) l
      (let* ((start (min i count))
             (end (min (+ i n) count)))
        (initialize-list type fill-elt (subseq store start end) (- end start)))) ))
  
(defmethod nth ((l persistent-list) (i integer))
  (with-slots (store) l
    (cl:nth i store)))

(defmethod index ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

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
;;;    A mutable list iterator will detect structural changes to the host list that occur independently
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
  ((list :initarg :list)
   (remote-control :initarg :remote-control))
  (:documentation "External iterator for a list. May traverse in either direction."))

(defmethod type ((i list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i list-iterator))
  (with-slots (list) i
    (emptyp list)))

;;
;;    Eliminate CURRENT in favor of NEXT/PREVIOUS? (Both move cursor and return elt. Error if at end?)
;;    
(defmethod current :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod current ((i list-iterator))
  (declare (ignore i))
  (error "LIST-ITERATOR does not implement CURRENT"))

(defgeneric current-index (iterator)
  (:documentation "Returns index of the current element of the iterator traversal."))
(defmethod current-index :around ((i list-iterator))
  (if (emptyp i)
      (error "List is empty")
      (call-next-method)))
(defmethod current-index ((i list-iterator))
  (declare (ignore i))
  (error "LIST-ITERATOR does not implement CURRENT-INDEX"))

(defgeneric (setf current) (obj list-iterator)
  (:documentation "Assign the object at the current iterator position."))
(defmethod (setf current) :around (obj (i list-iterator))
  (cond ((not (typep obj (type i))) (error "~A is not of type ~A" obj (type i)))
        ((emptyp i) (error "List is empty"))
        (t (call-next-method))))
(defmethod (setf current) (obj (i list-iterator))
  (declare (ignore i obj))
  (error "LIST-ITERATOR does not implement (SETF CURRENT)"))

(defgeneric has-next (iterator)
  (:documentation "Is there a next element for the iterator?"))
(defmethod has-next ((i list-iterator))
  (declare (ignore i))
  (error "LIST-ITERATOR does not implement HAS-NEXT"))

(defgeneric has-previous (iterator)
  (:documentation "Is there a previous element for the iterator?"))
(defmethod has-previous ((i list-iterator))
  (declare (ignore i))
  (error "LIST-ITERATOR does not implement HAS-PREVIOUS"))

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
  (error "LIST-ITERATOR does not implement PREVIOUS"))

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
  (error "LIST-ITERATOR does not implement REMOVE"))

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
  (error "LIST-ITERATOR does not implement ADD-BEFORE"))

(defgeneric add-after (iterator obj)
  (:documentation "Add an element to the list after the current element of the iterator."))
(defmethod add-after :around ((i list-iterator) obj)
  (if (not (typep obj (type i)))
      (error "~A is not of type ~A" obj (type i))
      (call-next-method)))
(defmethod add-after ((i list-iterator) obj)
  (declare (ignore i obj))
  (error "LIST-ITERATOR does not implement ADD-AFTER"))

;;;
;;;    MUTABLE-LIST-LIST-ITERATOR
;;;    
(defclass mutable-list-list-iterator (list-iterator)
  ((expected-modification-count :type integer))
  (:documentation "External iterator for a mutable list. May traverse in either direction."))

(defmethod initialize-instance :after ((i mutable-list-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (remote-control expected-modification-count) i
    (setf expected-modification-count (press remote-control modification-count))))

(defmethod count-modification ((i mutable-list-list-iterator))
  (with-slots (expected-modification-count) i
    (incf expected-modification-count)))

(defmethod co-modified ((i mutable-list-list-iterator))
  (with-slots (remote-control expected-modification-count) i
    (/= expected-modification-count (press remote-control modification-count))))

;;;
;;;    Lots of redundant checks! E.g., NEXT -> HAS-NEXT -> CURRENT
;;;    
(defmethod check-co-modification ((i mutable-list-list-iterator))
  (when (co-modified i)
    (error "List iterator invalid due to structural modification of list")))
  
(defmethod current :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod current ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement CURRENT"))

(defmethod current-index :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod current-index ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement CURRENT-INDEX"))

(defmethod (setf current) :around (obj (i mutable-list-list-iterator))
  (declare (ignore obj))
  (check-co-modification i)
  (call-next-method))
(defmethod (setf current) (obj (i mutable-list-list-iterator))
  (declare (ignore i obj))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement (SETF CURRENT)"))

(defmethod has-next :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod has-next ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement HAS-NEXT"))

(defmethod has-previous :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod has-previous ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement HAS-PREVIOUS"))

(defmethod next :around ((i mutable-list-list-iterator))
  (check-co-modification i) ; Already checked by HAS-NEXT...
  (call-next-method))
(defmethod next ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement NEXT"))

(defmethod previous :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod previous ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement PREVIOUS"))

(defmethod remove :around ((i mutable-list-list-iterator))
  (check-co-modification i)
  (call-next-method))
(defmethod remove ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement REMOVE"))
(defmethod remove :after ((i mutable-list-list-iterator))
  (count-modification i))

(defmethod add-before :around ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (check-co-modification i)
  (call-next-method))
(defmethod add-before ((i mutable-list-list-iterator) obj)
  (declare (ignore i obj))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement ADD-BEFORE"))
(defmethod add-before :after ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (count-modification i))

(defmethod add-after :around ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (check-co-modification i)
  (call-next-method))
(defmethod add-after ((i mutable-list-list-iterator) obj)
  (declare (ignore i obj))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement ADD-AFTER"))
(defmethod add-after :after ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (count-modification i))

;;;
;;;    Used by ARRAY-LIST and HASH-TABLE-LIST
;;;    
;;;
;;;    RANDOM-ACCESS-LIST-LIST-ITERATOR
;;;
(defclass random-access-list-list-iterator (mutable-list-list-iterator)
  ((cursor :type integer)))

(defmethod initialize-instance :after ((i random-access-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list cursor) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (setf cursor start)))

(defmethod current ((i random-access-list-list-iterator))
  (with-slots (list cursor) i
    (nth list cursor)))

(defmethod current-index ((i random-access-list-list-iterator))
  (slot-value i 'cursor))

(defmethod (setf current) (obj (i random-access-list-list-iterator))
  (with-slots (list) i
    (setf (nth list (current-index i)) obj)))

(defmethod next ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-next i)
           (incf cursor)
           (current i))
          (t nil))))

(defmethod previous ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-previous i)
           (decf cursor)
           (current i))
          (t nil))))

(defmethod has-next ((i random-access-list-list-iterator))
  (with-slots (list cursor) i
    (< cursor (1- (size list)))) )

(defmethod has-previous ((i random-access-list-list-iterator))
  (with-slots (cursor) i
    (> cursor 0)))

(defmethod remove ((i random-access-list-list-iterator))
  (with-slots (list cursor) i
    (let ((index cursor))
      (when (and (has-previous i)
                 (not (has-next i)))
        (decf cursor))
      (delete list index))))

(defmethod add-before ((i random-access-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i) (add list obj))
          (t (insert list cursor obj)
             (incf cursor)))) )

(defmethod add-after ((i random-access-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i) (add list obj))
          (t (insert list (1+ cursor) obj)))) )

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
(defclass singly-linked-list-list-iterator (mutable-list-list-iterator)
  ((index :type integer :initform 0)
   (cursor :type (or null cons))
   (history :initform (make-instance 'linked-stack))))

(defmethod initialize-instance :after ((i singly-linked-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (initialize-cursor i)
    (loop repeat start do (next i)))) ; Build initial history

;;;
;;;    CURSOR may be detached when:
;;;    1. List iterator is created on empty list
;;;    2. List becomes empty
;;;    
(defun initialize-cursor (list-iterator)
  (with-slots (remote-control cursor) list-iterator
    (setf cursor (press remote-control head-node))))

(defmethod current ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod current-index ((i singly-linked-list-list-iterator))
  (slot-value i 'index))

(defmethod (setf current) (obj (i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (setf (first cursor) obj)))

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

(defmethod previous ((i singly-linked-list-list-iterator))
  (with-slots (cursor index history) i
    (cond ((has-previous i)
           (setf cursor (pop history))
           (decf index)
           (current i))
          (t nil))))

(defmethod has-next ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (or (null cursor)
             (null (rest cursor)))) ))

(defmethod has-previous ((i singly-linked-list-list-iterator))
  (with-slots (list remote-control cursor) i
    (not (or (null cursor)
             (eq cursor (press remote-control head-node)))) ))

(defmethod remove ((i singly-linked-list-list-iterator))
  (with-slots (list cursor index history) i
    (cond ((zerop index)
           (prog1 (delete-node list cursor)
             (initialize-cursor i))) ; Allow GC
          (t (let ((parent (peek history)))
               (cond ((has-next i) (setf cursor (rest cursor)))
                     (t (setf cursor (pop history))
                        (decf index)))
               (delete-child list parent)))) ))

(defmethod add-before ((i singly-linked-list-list-iterator) obj)
  (with-slots (list cursor index history) i
    (cond ((emptyp i)
           (add list obj)
           (initialize-cursor i))
          (t (insert-before list cursor obj)
             (push history cursor)
             (cl:pop cursor)
             (incf index)))) )

(defmethod add-after ((i singly-linked-list-list-iterator) obj)
  (with-slots (list cursor index) i
    (cond ((emptyp i)
           (add list obj)
           (initialize-cursor i))
          (t (insert-after list cursor obj)))) )

;;;
;;;    DOUBLY-LINKED-LIST-LIST-ITERATOR
;;;    
(defclass doubly-linked-list-list-iterator (mutable-list-list-iterator)
  ((cursor :type dcursor)))

(defmethod initialize-instance :after ((i doubly-linked-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list remote-control cursor) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (setf cursor (press remote-control initialize))
    (unless (zerop start)
      (advance cursor start))))

(defmethod current ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (content (slot-value cursor 'node)))) ; ???

(defmethod current-index ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (slot-value cursor 'index))) ; ??? <-----------------------

(defmethod (setf current) (obj (i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (with-slots (node) cursor ; ???
      (setf (content node) obj))))

(defmethod has-next ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-end-p cursor))))

(defmethod has-previous ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-start-p cursor))))

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

(defmethod remove ((i doubly-linked-list-list-iterator))
  (with-slots (list cursor) i
    (with-slots (index node) cursor ; ???
      (cond ((zerop index)
             (prog1 (delete-node list node)
               (reset cursor))) ; Allow GC
            (t (let ((current-node node))
                 (cond ((has-next i) (bump cursor))
                       (t (rewind cursor)))
                 (delete-node list current-node)))) )))

(defmethod add-before ((i doubly-linked-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i)
           (add list obj)
           (reset cursor))
          (t (with-slots (node) cursor
               (insert-before list node obj)
               (nudge cursor)))) ))

(defmethod add-after ((i doubly-linked-list-list-iterator) obj)
  (with-slots (list cursor) i
    (cond ((emptyp i)
           (add list obj)
           (reset cursor))
          (t (with-slots (node) cursor ; ???
               (insert-after list node obj)))) ))

;;;
;;;    PERSISTENT-LIST-LIST-ITERATOR
;;;    - Must be able to retrieve associated list after structural modifications...
;;;      "Modification" of list iterator creates new list iterator associated with new list!
;;;    - NEXT/PREVIOUS should return secondary value consisting of list elt??
;;;
;; (defclass persistent-list-list-iterator (list-iterator)
;; ;  ((list :initarg :list)
;;   ((list :initarg :list :reader list)
;;    (index :type integer :initarg :index :initform 0)
;;    (cursor :initarg :cursor :initform '() :type (or null cons))
;;    (history :initarg :history :initform (make-instance 'persistent-stack :type cons))))

(let ((empty-history (make-instance 'persistent-stack :type 'cons)))
  (defclass persistent-list-list-iterator (list-iterator)
;    ((list :initarg :list :reader list) ; ????????????????? <-- This is #'list?!
;    ((list :initarg :list)
    ((index :type integer :initform 0)
     (cursor :type cl:list)
     (history :initform empty-history))))

;; (defmethod initialize-instance :after ((i persistent-list-list-iterator) &rest initargs &key (start 0))
;;   (declare (ignore initargs))
;;   (with-slots (list index cursor) i
;;     (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
;;     (
    ;; (when (null cursor)
    ;;   (initialize-persistent-cursor i))))
;    (loop repeat start do (next i)))) ; Build initial history

(defmethod initialize-instance :after ((i persistent-list-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (remote-control cursor) i
    (setf cursor (press remote-control head-node))))

;;;
;;;    Refactor:
;;;    - Make this tail-recursive
;;;    - Improve efficiency of creating list iterator (less garbage)
;;;    - Improve efficiency of creating history stack (less garbage). Provide content directly
;;;    - START too big?!
;;;        (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
;;;
(defmethod make-instance :around ((class (eql (find-class 'persistent-list-list-iterator))) &rest initargs &key (start 0))
  (cond ((minusp start) (error "Invalid index: ~D" start))
        ((zerop start)
         (remf initargs :start) ; :START is not legal for next method?!?
         (apply #'call-next-method class initargs))
        (t (setf (getf initargs :start) (1- start))
           (next (apply #'make-instance class initargs)))) )

;;;
;;;    CURSOR may be detached when:
;;;    1. List iterator is created on empty list
;;;    2. List becomes empty
;;;    
;; (defun initialize-persistent-cursor (list-iterator)
;;   (with-slots (list cursor) list-iterator
;;     (setf cursor (slot-value list 'store))))
  
(defmethod current ((i persistent-list-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod current-index ((i persistent-list-list-iterator))
  (slot-value i 'index))

;;;
;;;    History becomes invalidated.
;;;    
;;;
;;;    SETF method need not actually set anything???
;;;    Simply used for value...
;;;    
(defmethod (setf current) (obj (i persistent-list-list-iterator))
  (with-slots (list index) i
    (list-iterator (setf (nth list index) obj) index)))

(defmethod remove ((i persistent-list-list-iterator))
  (with-slots (list index) i
    (multiple-value-bind (new-list doomed) (delete list index)
      (values (list-iterator new-list (min index (1- (size new-list))))
              doomed))))

(defmethod add-before ((i persistent-list-list-iterator) obj)
  (with-slots (list index) i
    (cond ((emptyp i) (list-iterator (add list obj)))
          (t (list-iterator (insert list index obj) (1+ index)))) ))

(defmethod add-after ((i persistent-list-list-iterator) obj)
  (with-slots (list index) i
    (cond ((emptyp i) (list-iterator (add list obj)))
          (t (list-iterator (insert list (1+ index) obj) index)))) )

(defmethod has-next ((i persistent-list-list-iterator))
  (with-slots (cursor) i
    (not (null (rest cursor)))) )

(defmethod has-previous ((i persistent-list-list-iterator))
  (with-slots (history) i
    (not (emptyp history))))

(flet ((initialize-iterator (iterator index cursor history)
         (with-slots (list remote-control) iterator
           (let ((new-iterator (make-instance 'persistent-list-list-iterator
                                              :list list
                                              :remote-control remote-control)))
             (with-slots ((new-index index) (new-cursor cursor) (new-history history)) new-iterator
               (setf new-cursor cursor
                     new-index index
                     new-history history))
             new-iterator))))
  (defmethod next ((i persistent-list-list-iterator))
    (with-slots (index cursor history) i
      (cond ((has-next i)
             (let ((next-iterator (initialize-iterator i (1+ index) (rest cursor) (push history cursor))))
               (values next-iterator (current next-iterator))))
            (t nil))))  ;<-------------------
  (defmethod previous ((i persistent-list-list-iterator))
    (with-slots (index cursor history) i
      (cond ((has-previous i)
             (let ((previous-iterator (initialize-iterator i (1- index) (peek history) (pop history))))
               (values previous-iterator (current previous-iterator))))
            (t nil)))) ) ;<-------------------

;;;
;;;    If not for 2 problems, the following implementations of NEXT and PREVIOUS would be ideal:
;;;    1. These are inefficient since the history has to be rebuilt from the beginning each time.
;;;       The above versions use the current history as the basis for list iterator produced.
;;;    2. More seriously, the START parameter in the MAKE-INSTANCE :AROUND calls NEXT which triggers
;;;       an infinite cascade of recursive calls! PREVIOUS below is Ok.
;;;       
;; (defmethod next ((i persistent-list-list-iterator))
;;   (cond ((has-next i)
;;          (with-slots (list index) i
;;            (let ((next-iterator (list-iterator list (1+ index))))
;;              (values next-iterator (current next-iterator)))) )
;;         (t nil)))

;; (defmethod previous ((i persistent-list-list-iterator))
;;   (cond ((has-previous i)
;;          (with-slots (list index) i
;;            (let ((previous-iterator (list-iterator list (1- index))))
;;              (values previous-iterator (current previous-iterator)))) )
;;         (t nil)))


                    
