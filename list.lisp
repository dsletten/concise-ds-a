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
;;;    Regarding return value.
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
  (error "LIST does not implement SLICE"))

;;;
;;;    MUTABLE-LIST
;;;
(defclass mutable-list (mutable-collection list)
  ()
  (:documentation "A list whose structure and elements can be modified."))

(defmethod clear :after ((l mutable-list))
  (count-modification l))           

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
;;;    LINKED-LIST
;;;
;;;    These methods are dangerous? We can't verify that the given NODE is actually
;;;    part of the list structure? (Without expensive linear search which defeats the
;;;    whole point of these methods...)
;;;
;;;    Don't export names. DELETE (public) renamed to DELETE-NODE/DELETE-CHILD
;;;    
(defclass linked-list (list)
  ()
  (:documentation "A list implemented with linked nodes."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Structural modification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric insert-before (list node obj)
  (:documentation "Insert the object before the specified node."))
(defmethod insert-before :around ((l linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-before ((l linked-list) node obj)
  (declare (ignore l node obj))
  (error "LINKED-LIST does not implement INSERT"))

(defgeneric insert-after (list node obj)
  (:documentation "Insert the object after the specified node."))
(defmethod insert-after :around ((l linked-list) node obj)
  (cond ((not (typep obj (type l))) (error "~A is not of type ~A" obj (type l)))
        ((null node) (error "Invalid node"))
        (t (call-next-method))))
(defmethod insert-after ((l linked-list) node obj)
  (declare (ignore l node obj))
  (error "LINKED-LIST does not implement INSERT"))

(defgeneric delete-node (list doomed)
  (:documentation "Delete the specified node from the list."))
(defmethod delete-node :around ((l linked-list) doomed)
  (declare (ignore l))
  (if (null doomed)
      (error "Invalid node")
      (call-next-method)))
(defmethod delete-node ((l linked-list) doomed)
  (declare (ignore l doomed))
  (error "LINKED-LIST does not implement DELETE-NODE"))

(defgeneric delete-child (list parent)
  (:documentation "Delete the child of the specified node from the list."))
(defmethod delete-child :around ((l linked-list) parent)
  (declare (ignore l))
  (if (null parent)
      (error "Invalid node")
      (call-next-method)))
(defmethod delete-child ((l linked-list) parent)
  (declare (ignore l parent))
  (error "LINKED-LIST does not implement DELETE-child"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    MUTABLE-LINKED-LIST
;;;
(defclass mutable-linked-list (mutable-list linked-list)
  ()
  (:documentation "A list implemented with linked nodes whose structure and elements can be modified."))

(defmethod insert-before :after ((l mutable-linked-list) node obj)
  (declare (ignore node obj))
  (count-modification l))

(defmethod insert-after :after ((l mutable-linked-list) node obj)
  (declare (ignore node obj))
  (count-modification l))

(defmethod delete-node :after ((l mutable-linked-list) doomed)
  (declare (ignore doomed))
  (count-modification l))

(defmethod delete-child :after ((l mutable-linked-list) parent)
  (declare (ignore parent))
  (count-modification l))

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

(defmethod iterator ((l array-list))
  (make-instance 'random-access-list-iterator :collection l))

(defmethod list-iterator ((l array-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l array-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

(defmethod add ((l array-list) &rest objs)
  (unless (null objs)
    (with-slots (store) l
      (dolist (obj objs) ; Allocate new array?
        (vector-push-extend obj store)))) )

;;;
;;;    i < -size => error or no effect?
;;;    Add elt to end: (insert al (size al) x)
;;;    Can't do this with negative index
;;;    
(defmethod insert ((l array-list) (i integer) obj)
  (with-slots (store) l
    (vector-push-extend (fill-elt l) store)
    (setf (subseq store (1+ i)) (subseq store i)
          (aref store i) obj)))

(defmethod delete ((l array-list) (i integer))
  (with-slots (store) l
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
  (make-instance 'random-access-list-iterator :collection l))

(defmethod list-iterator ((l array-list-x) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l array-list-x) obj &key (test #'eql))
  (with-slots (store offset) l
    (find obj store :start offset :test test)))

(defmethod add ((l array-list-x) &rest objs)
  (unless (null objs)
    (with-slots (store) l
      (dolist (obj objs) ; Allocate new array?
        (vector-push-extend obj store)))) )

(defmethod insert ((l array-list-x) (i integer) obj)
  (with-slots (store offset) l
    (let ((j (+ i offset)))
      (cond ((or (zerop offset)
                 (> i (floor (size l) 2)))
             (vector-push-extend (fill-elt l) store)
             (setf (subseq store (1+ j)) (subseq store j)))
            (t (decf offset)
               (setf (subseq store offset) (subseq store (1+ offset) j))))
      (setf (aref store (+ i offset)) obj))))

(defmethod delete ((l array-list-x) (i integer))
  (with-slots (store offset fill-elt) l
    (let ((j (+ i offset)))
      (prog1 (aref store j)
        (cond ((<= i (floor (size l) 2))
               (setf (subseq store (1+ offset)) (subseq store offset j)
                     (aref store offset) fill-elt)
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

(defmethod slice ((l array-list-x) (i integer) (n integer))
  (with-slots (store offset) l
    (let ((al (make-array-list-x :type (type l) :fill-elt (fill-elt l)))
          (count (size l)))
      (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (aref store (+ k offset))))
      al)))

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
  (make-instance 'random-access-list-iterator :collection l))

(defmethod list-iterator ((l array-list-x) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l array-list-x) obj &key (test #'eql))
  (with-slots (store offset) l
    (find obj store :start offset :test test)))

(defmethod add ((l array-list-x) &rest objs)
  (unless (null objs)
    (with-slots (store) l
      (dolist (obj objs)
        (vector-push-extend obj store)))) )

(defmethod insert ((l array-list-x) (i integer) obj)
  (with-slots (store offset) l
    (cond ((zerop offset)
           (vector-push-extend (fill-elt l) store)
           (setf (subseq store (1+ i)) (subseq store i)
                 (aref store i) obj))
          (t (decf offset)
             (setf (subseq store offset (+ i offset)) (subseq store (1+ offset))
                   (aref store (+ i offset)) obj)))) )

(defmethod delete ((l array-list-x) (i integer))
  (with-slots (store offset fill-elt) l
    (prog1 (aref store (+ i offset))
      ;; (cond ((= offset i 0) ; This doesn't make a bit of difference?!
      ;;        (setf (aref store 0) fill-elt)
      ;;        (incf offset))
      (cond ((<= (+ i offset) (floor (size l) 2))
             (setf (subseq store (1+ offset)) (subseq store offset (+ i offset))
                   (aref store offset) fill-elt)
             (incf offset))
            (t (setf (subseq store (+ i offset)) (subseq store (1+ (+ i offset))))
               (vector-pop store)))) ))

(defmethod nth ((l array-list-x) (i integer))
  (with-slots (store offset) l
    (aref store (+ i offset))))

(defmethod (setf nth) (obj (l array-list-x) (i integer))
  (with-slots (store offset) l
    (setf (aref store (+ i offset)) obj)))

(defmethod index ((l array-list-x) obj &key (test #'eql))
  (with-slots (store offset) l
    (let ((i (position obj store :start offset :test test)))
      (if (null i)
          i
          (- i offset)))) )

(defmethod slice ((l array-list-x) (i integer) (n integer))
  (with-slots (store offset) l
    (let ((al (make-array-list-x :type (type l) :fill-elt (fill-elt l)))
          (count (size l)))
      (apply #'add al (loop for k from (min i count) below (min (+ i n) count) collect (aref store (+ k offset))))
      al)))

;;;
;;;    Used by ARRAY-LIST and HASH-TABLE-LIST
;;;    
(defclass random-access-list-iterator (mutable-collection-iterator)
  ((cursor :initform 0 :type integer)))

(defmethod current ((i random-access-list-iterator))
  (with-slots (collection cursor) i
    (nth collection cursor)))

(defmethod next ((i random-access-list-iterator))
  (with-slots (cursor) i
    (cond ((done i) nil)
          (t (incf cursor)
             (if (done i)
                 nil
                 (current i)))) ))

(defmethod done ((i random-access-list-iterator))
  (with-slots (collection cursor) i
    (assert (<= cursor (size collection)) () "Index is out of bounds: ~D" cursor)
    (= cursor (size collection))))

;;;
;;;    SINGLY-LINKED-LIST
;;;    
(defclass singly-linked-list (mutable-linked-list)
  ((store :accessor store :initform '())
   (count :initform 0)))

(defun make-linked-list (&key (type t) (fill-elt nil))
  (make-instance 'singly-linked-list :type type :fill-elt fill-elt))

(defmethod size ((l singly-linked-list))
  (slot-value l 'count))

(defmethod emptyp ((l singly-linked-list))
  (null (store l)))

(defmethod clear ((l singly-linked-list))
  (with-slots (store count) l
    (setf store '()
          count 0)))

(defmethod iterator ((l singly-linked-list))
  (make-instance 'singly-linked-list-iterator :collection l))

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
    (with-slots (store count) l
      (loop for i from 0
            for elt in objs
            collect elt into elts
            finally (progn (setf store (nconc store elts))
                           (incf count i)))) ))

(defmethod insert ((l singly-linked-list) (i integer) obj)
  (with-slots (store) l
    (insert-cons-before (nthcdr i store) obj)))
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
  (insert-cons-before node obj))
(defmethod insert-before :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

;;;
;;;    Make copy of current node. Update current node to
;;;    become "previous" node in place.
;;;    
(defun insert-cons-before (node obj)
  (let ((copy (cons (first node) (rest node))))
    (setf (first node) obj
          (rest node) copy)))

(defmethod insert-after ((l singly-linked-list) node obj)
  (declare (ignore l))
  (insert-cons-after node obj))
(defmethod insert-after :after ((l singly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defun insert-cons-after (node obj)
  (let ((tail (cons obj (rest node))))
    (setf (rest node) tail)))

(defmethod delete ((l singly-linked-list) (i integer))
  (with-slots (store) l
    (if (zerop i)
        (delete-cons-node l store)
        (delete-cons-child (nthcdr (1- i) store)))) )
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
  (delete-cons-node l doomed))
(defmethod delete-node :after ((l singly-linked-list) (doomed cons))
  (declare (ignore doomed))
  (with-slots (count) l
    (decf count)))

(defun delete-cons-node (l doomed)
  (let ((content (first doomed))
        (saved (rest doomed)))
    (prog1 content
      (cond ((eq doomed (store l)) ; First elt
             (setf (store l) saved))
            ((null saved) ; Can't delete last node (unless only elt)
             (error "Current node must have non-nil next node"))
            (t (setf (first doomed) (first saved)
                     (rest doomed) (rest saved)))) )))

;;;    DELETE-CHILD cannot delete 1st node in list!
;;;    - Route around child.
;;;    - PARENT cannot itself be last elt.
(defmethod delete-child ((l singly-linked-list) (parent cons))
  (delete-cons-child parent))
(defmethod delete-child :after ((l singly-linked-list) (parent cons))
  (declare (ignore parent))
  (with-slots (count) l
    (decf count)))

(defun delete-cons-child (parent)
  (let ((child (rest parent)))
    (if (null child)
        (error "Parent must have child node")
        (prog1 (first child)
          (setf (rest parent) (rest child)))) ))

(defmethod nth ((l singly-linked-list) (i integer))
  (with-slots (store) l
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
;;;    SINGLY-LINKED-LIST-X (TCONC)
;;;    - Keeping track of the rear makes ADD _much_ faster!
;;;    
(defclass singly-linked-list-x (mutable-linked-list)
  ((front :accessor store :initform nil) ; Weird accessor
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
  (null (store l)))

(defmethod clear ((l singly-linked-list-x))
  (with-slots (front rear count) l
    (setf front nil
          rear nil
          count 0)))

(defmethod iterator ((l singly-linked-list-x))
  (make-instance 'singly-linked-list-iterator :collection l))

;;; ??
(defmethod list-iterator ((l singly-linked-list-x) &optional (start 0))
  (make-instance 'singly-linked-list-list-iterator :list l :start start))

(defmethod contains ((l singly-linked-list-x) obj &key (test #'eql))
  (with-slots (front) l
    (find obj front :test test)))

;;;
;;;    Copy OBJS list and find its length in one traversal.
;;;    
(defmethod add ((l singly-linked-list-x) &rest objs)
  (unless (null objs)
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
              (t (add-nodes objs)))) )))

(defmethod insert ((l singly-linked-list-x) (i integer) obj)
  (with-slots (front) l
    (let ((node (nthcdr i front)))
      (insert-cons-before-x l node obj))))
(defmethod insert :after ((l singly-linked-list-x) (i integer) obj)
  (declare (ignore i obj))
  (with-slots (count) l
    (incf count)))

(defmethod insert-before ((l singly-linked-list-x) node obj)
  (insert-cons-before-x l node obj))
(defmethod insert-before :after ((l singly-linked-list-x) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defun insert-cons-before-x (l node obj)
  "Update NODE to become 'previous' node in place. Move REAR as necessary."
  (with-slots (rear) l
    (insert-cons-before node obj)
    (when (eq node rear)
      (setf rear (rest rear)))) )
  
(defmethod insert-after ((l singly-linked-list-x) node obj)
  (insert-cons-after-x l node obj))
(defmethod insert-after :after ((l singly-linked-list-x) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defun insert-cons-after-x (l node obj)
  (with-slots (rear) l
    (insert-cons-after node obj)
    (when (eq node rear)
      (setf rear (rest rear)))) )

(defmethod delete ((l singly-linked-list-x) (i integer))
  (with-slots (front) l
    (cond ((zerop i) (delete-cons-node-x l front))
          (t (delete-cons-child-x l (nthcdr (1- i) front)))) ))
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
  (delete-cons-node-x l doomed))
(defmethod delete-node :after ((l singly-linked-list-x) (doomed cons))
  (declare (ignore doomed))
  (with-slots (count) l
    (decf count)))

(defun delete-cons-node-x (l doomed)
  (with-slots (front rear) l
    (prog1 (delete-cons-node l doomed)
      (when (null front)
	(setf rear nil)))) )

;;;    DELETE-CHILD cannot delete 1st node in list!
;;;    - Route around child.
;;;    - PARENT cannot itself be last elt.
(defmethod delete-child ((l singly-linked-list-x) (parent cons))
  (delete-cons-child-x l parent))
(defmethod delete-child :after ((l singly-linked-list-x) (parent cons))
  (declare (ignore parent))
  (with-slots (count) l
    (decf count)))

(defun delete-cons-child-x (l parent)
  (with-slots (rear) l
    (prog1 (delete-cons-child parent)
      (when (null (rest parent))
        (setf rear parent)))) )

(defmethod nth ((l singly-linked-list-x) (i integer))
  (with-slots (front) l
    (cl:nth i front)))

(defmethod (setf nth) (obj (l singly-linked-list-x) (i integer))
  (with-slots (front) l
    (setf (cl:nth i front) obj)))

(defmethod index ((l singly-linked-list-x) obj &key (test #'eql))
  (with-slots (front) l
    (position obj front :test test)))

(defmethod slice ((l singly-linked-list-x) (i integer) (n integer))
  (with-slots (front count) l
    (let ((sllx (make-linked-list-x :type (type l) :fill-elt (fill-elt l))))
      (apply #'add sllx (subseq front (min i count) (min (+ i n) count)))
      sllx)))

;;;
;;;    Don't need to hold onto LIST after initialization?
;;;    CURSOR added to avoid manipulating LIST directly...
;;;    
(defclass singly-linked-list-iterator (mutable-collection-iterator)
  ((cursor)))

(defmethod initialize-instance :after ((i singly-linked-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (cursor collection) i
;    (setf cursor (slot-value collection 'store)))) ; Inappropriate access?
    (setf cursor (store collection))))

(defmethod current ((i singly-linked-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod next ((i singly-linked-list-iterator))
  (with-slots (cursor) i
    (cond ((done i) nil)
          (t (cl:pop cursor)
             (if (done i)
                 nil
                 (current i)))) ))

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

(defmethod print-object ((dcons dcons) stream)
  (print-unreadable-object (dcons stream)
    (print-previous stream dcons)
    (format stream "~A" (content dcons))
    (print-next stream dcons)))

(defun print-previous (stream dcons)
  (cond ((null (previous dcons)) (format stream "∅ ← "))
        ((eq dcons (previous dcons)) (format stream "↻ "))
        (t (format stream "~A ← " (content (previous dcons)))) ))

(defun print-next (stream dcons)
  (cond ((null (next dcons)) (format stream " → ∅"))
        ((eq dcons (next dcons)) (format stream " ↺"))
        (t (format stream " → ~A" (content (next dcons)))) ))

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
;;;    - Can't make doubly-linked tree? CONTENT of DCONS is singly-linked.
;;;    
(defclass doubly-linked-list (mutable-linked-list)
  ((store :initform nil)
   (count :initform 0)
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
  (make-instance 'doubly-linked-list-iterator :collection l))

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
          (t (with-slots (index node) cursor
               (declare (integer index))
               (cond ((zerop i) store)
                     ((= i index) node)
                     ((< i (/ index 2))
                      (reset cursor)
                      (advance cursor i)
                      node)
                     ((< i index)
                      (rewind cursor (- index i))
                      node)
                     ((<= i (/ (+ count index) 2))
                      (advance cursor (- i index))
                      node)
                     (t (reset cursor)
                        (rewind cursor (- count i))
                        node)))) )))

(defmethod insert ((l doubly-linked-list) (i integer) obj)
  (insert-dcons-before l (nth-dcons l i) obj))
(defmethod insert :after ((l doubly-linked-list) (i integer) obj)
  (declare (ignore obj))
  (with-slots (count cursor) l
    (incf count)
    (with-slots (index) cursor
      (when (or (not (initializedp cursor))
                (<= 0 i index)
                (and (minusp i) (<= 0 (+ i count) index)))
        (reset cursor)))) )

;;;
;;;    Is NODE actually part of the structure of L???
;;;    - If not, MODIFICATION-COUNT, COUNT, CURSOR are modified inappropriately.
;;;    
(defmethod insert-before ((l doubly-linked-list) node obj)
  (insert-dcons-before l node obj))
(defmethod insert-before :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count cursor) l
    (incf count)
    (with-slots (index) cursor
      (incf index))))

(defun insert-dcons-before (l node obj)
  (with-slots (store) l
    (let ((new-dcons (make-instance 'dcons :content obj)))
      (dlink (previous node) new-dcons)
      (dlink new-dcons node)
      (when (eq store node)
        (setf store new-dcons)))) )
  
(defmethod insert-after ((l doubly-linked-list) node obj)
  (let ((new-dcons (make-instance 'dcons :content obj)))
    (dlink new-dcons (next node)) ; Do this in the right order!!
    (dlink node new-dcons)))
(defmethod insert-after :after ((l doubly-linked-list) node obj)
  (declare (ignore node obj))
  (with-slots (count) l
    (incf count)))

(defmethod delete ((l doubly-linked-list) (i integer))
  (delete-dcons-node l (nth-dcons l i)))
(defmethod delete :after ((l doubly-linked-list) (i integer))
  (declare (ignore i))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor)))
;;;
;;;    Confirm that DOOMED is actually a node in this list!
;;;    - Smuggling in some fabricated DCONS could snip out multiple nodes depending
;;;      on what its NEXT and PREVIOUS links pointed to.
;;;      
;;;    This method needs to be absolutely private! Risky to split an object up like this...
;;;    

(defmethod delete-node ((l doubly-linked-list) (doomed dcons))
  (delete-dcons-node l doomed))
(defmethod delete-node :after ((l doubly-linked-list) (doomed dcons))
  (declare (ignore doomed))
  (with-slots (count cursor) l
    (decf count)
    (reset cursor))) ; NTH-DCONS moves cursor in most cases?

(defun delete-dcons-node (l doomed)
  (with-slots (store) l
    (let ((content (content doomed))
          (saved (next doomed)))
    (prog1 content
      (cond ((eq doomed saved) ; Single-elt
             (setf store '()))
            ((eq doomed store) ; First elt otherwise
             (setf (content doomed) (content saved))
             (dlink doomed (next saved)))
            (t (dlink (previous doomed) saved)))) ))) ; Other than first elt
  
;;;
;;;    DELETE-CHILD not really necessary for DOUBLY-LINKED-LIST.
;;;    
(defmethod delete-child ((l doubly-linked-list) (parent dcons))
  (with-slots (store) l
    (let ((child (next parent)))
      (if (eq child store)
          (error "Parent must have child node")
          (prog1 (content child)
            (dlink parent (next child)))) )))
(defmethod delete-child :after ((l doubly-linked-list) (doomed dcons))
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

(defclass doubly-linked-list-iterator (mutable-collection-iterator)
  ((cursor)
   (sealed-for-your-protection :initform t)))

(defmethod initialize-instance :after ((i doubly-linked-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (cursor collection) i
    (setf cursor (make-instance 'dcursor :list collection))))

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
  (make-instance 'random-access-list-iterator :collection l))

(defmethod list-iterator ((l hash-table-list) &optional (start 0))
  (make-instance 'random-access-list-list-iterator :list l :start start))

(defmethod contains ((l hash-table-list) obj &key (test #'eql))
  (with-slots (store) l
    (dotimes (i (size l) nil)
      (when (funcall test obj (gethash i store))
        (return (gethash i store)))) ))

(defmethod add ((l hash-table-list) &rest objs)
  (unless (null objs)
    (with-slots (store) l
      (loop for i from (size l)
            for obj in objs
            do (setf (gethash i store) obj)))) )

(defmethod insert ((l hash-table-list) (i integer) obj)
  (with-slots (store) l
    (loop for j from (size l) above i
          do (setf (gethash j store) (gethash (1- j) store)))
    (setf (gethash i store) obj)))
                   
(defmethod delete ((l hash-table-list) (i integer))
  (with-slots (store) l
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
    (let ((htl (make-hash-table-list :type (type l) :fill-elt (fill-elt l)))
          (count (size l)))
      (apply #'add htl (loop for k from (min i count) below (min (+ i n) count) collect (gethash k store)))
      htl)))

;;;
;;;    PERSISTENT-LIST
;;;    - No point in making this a subclass of LINKED-LIST.
;;;      No advantage to having reference to "current" node since structure must be rebuilt.
;;; 
(defclass persistent-list (list)
  ((store :initform '() :initarg :store)
;   (count :initform 0 :initarg :count))) ; Should we trust this??
   (count))) ; No!

(defmethod initialize-instance :after ((l persistent-list) &rest initargs)
  (declare (ignore initargs))
  (with-slots (store count) l
    (setf count (length store))))

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
  (make-instance 'persistent-list :type (type l) :fill-elt (fill-elt l)))

(defmethod iterator ((l persistent-list))
  (make-instance 'persistent-list-iterator :collection l))

(defmethod list-iterator ((l persistent-list) &optional (start 0))
  (make-instance 'persistent-list-list-iterator :list l :cursor (slot-value l 'store) :start start))

(defmethod contains ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (find obj store :test test)))

(defmethod add ((l persistent-list) &rest objs)
  (if (null objs)
      l
      (with-slots (store) l
        (loop for elt in objs
              collect elt into elts
              finally (return (make-instance 'persistent-list 
                                             :store (append store elts) 
                                             :type (type l)
                                             :fill-elt (fill-elt l)))) )))

;; (defmethod insert ((l persistent-list) (i integer) obj)
;;   (with-slots (store) l
;;     (insert-before l (nthcdr i store) obj)))
(defmethod insert ((l persistent-list) (i integer) obj)
  (with-slots (store) l
    (make-instance 'persistent-list
                   :store (loop for elt in store
                                for cons on store
                                repeat i ; This has to be here!
                                collect elt into elts
                                finally (return (nconc elts (cons obj cons))))
                   :type (type l)
                   :fill-elt (fill-elt l))))

;; (defmethod insert-before ((l persistent-list) (node cons) obj)
;;   (with-slots (store) l
;;     (let ((new-store (loop for elt in store
;;                            for cons on store
;;                            until (eq node cons)
;;                            collect elt into elts
;;                            finally (return (if (eq node cons)
;;                                                (nconc elts (cons obj cons))
;;                                                store)))) )
;;       (if (eq new-store store)
;;           l
;;           (make-instance 'persistent-list
;;                          :store new-store
;;                          :type (type l)
;;                          :fill-elt (fill-elt l)))) ))

;; (defmethod insert-after ((l persistent-list) (node cons) obj)
;;   (with-slots (store) l
;;     (let ((new-store (loop for elt in store
;;                            for cons on store
;;                            until (eq node cons)
;;                            collect elt into elts
;;                            finally (return (if (eq node cons)
;;                                                (nconc elts (cons elt (cons obj (rest cons))))
;;                                                store)))) )
;;       (if (eq new-store store)
;;           l
;;           (make-instance 'persistent-list
;;                          :store new-store
;;                          :type (type l)
;;                          :fill-elt (fill-elt l)))) ))

(defmethod delete :around ((l persistent-list) (i integer))
  (cond ((emptyp l) (error "List is empty")) ; ??????
        ((>= i (size l)) l)
        ((< i (- (size l))) l)
        (t (call-next-method))))
;; (defmethod delete ((l persistent-list) (i integer))
;;   (with-slots (store) l
;;     (delete-node l (nthcdr i store))))
(defmethod delete ((l persistent-list) (i integer))
  (with-slots (store) l
    (multiple-value-bind (new-store doomed)
        (loop for elt in store
              ;; for tail on (rest store)
              ;; repeat i ; This has to be here!
              ;; collect elt into elts
              ;; finally (return (values (nconc elts tail) elt)))
              for tail on store
              repeat i ; This must be here.
              collect elt into elts
              finally (return (values (nconc elts (rest tail)) elt)))
      (values (make-instance 'persistent-list
                             :store new-store
                             :type (type l)
                             :fill-elt (fill-elt l))
              doomed))))

;;;
;;;    Need to test DELETE-NODE/DELETE-CHILD directly.
;;;    - Only used by list iterator. Only 1 branch of each called?
;;;
;;;    This method _can_ remove last node (unlike mutable list) since list is being rebuilt.
;;;    
;; (defmethod delete-node ((l persistent-list) (doomed cons))
;;   (with-slots (store) l
;;     (let ((new-store (loop for elt in store
;;                            for tail on store
;;                            until (eq tail doomed)
;;                            collect elt into elts
;;                            finally (return (if (eq tail doomed)
;;                                                (nconc elts (rest tail))
;;                                                store)))) )
;;       (if (eq new-store store)
;;           (values l nil)
;;           (values (make-instance 'persistent-list
;;                                  :store new-store
;;                                  :type (type l)
;;                                  :fill-elt (fill-elt l))
;;                   (first doomed)))) ))

;; ;;;
;; ;;;    Not really needed.
;; ;;;    
;; (defmethod delete-child ((l persistent-list) (parent cons))
;;   (let ((child (rest parent)))
;;     (if (null child)
;;         (error "Parent must have child node")
;;         (delete-node l child))))

(defmethod nth ((l persistent-list) (i integer))
  (with-slots (store) l
    (cl:nth i store)))

;;;
;;;    SETF method need not actually set anything???
;;;    Simply used for value...
;;;    
(defmethod (setf nth) (obj (l persistent-list) (i integer))
  (with-slots (store) l
    (if (zerop i) ; Why 2 cases?! Can't get LOOP to work for both...
        (make-instance 'persistent-list
                       :store (cons obj (rest store))
                       :type (type l)
                       :fill-elt (fill-elt l))
        (make-instance 'persistent-list
                       :store (loop repeat i
                                    for elt in store
                                    for tail on (rest store)
                                    collect elt into elts
                                    finally (return (nconc elts (cons obj (rest tail)))) )
                       :type (type l)
                       :fill-elt (fill-elt l)))) )

(defmethod index ((l persistent-list) obj &key (test #'eql))
  (with-slots (store) l
    (position obj store :test test)))

(defmethod slice ((l persistent-list) (i integer) (n integer))
  (with-slots (store count) l
    (let* ((start (min i count))
           (end (min (+ i n) count)))
      (make-instance 'persistent-list
                     :store (subseq store start end)
                     :type (type l)
                     :fill-elt (fill-elt l)))) )

;;;
;;;    Don't need to hold onto LIST after initialization?
;;;    CURSOR added to avoid manipulating LIST directly...
;;;    
;;;
;;;    Identical to SINGLY-LINKED-LIST-ITERATOR?!
;;;    Iterator should be persistent too!
;;;    
(defclass persistent-list-iterator (iterator)
  ((collection :initarg :collection)))

(defmethod current ((i persistent-list-iterator))
  (with-slots (collection) i
    (nth collection 0)))

(defmethod next ((i persistent-list-iterator))
  (with-slots (collection) i
    (cond ((done i) i)
          (t (let ((new-iterator (make-instance 'persistent-list-iterator :collection (delete collection 0))))
               (if (done new-iterator)
                   (values new-iterator nil)
                   (values new-iterator (current new-iterator)))) ))))

(defmethod done ((i persistent-list-iterator))
  (with-slots (collection) i
    (emptyp collection)))

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
  (error "LIST-ITERATOR does not implement TYPE"))

;;; True for single-element list too!!!
;; (defmethod emptyp ((i list-iterator))
;;   (not (or (has-next i)
;;            (has-previous i))))
(defmethod emptyp ((i list-iterator))
  (declare (ignore i))
  (error "LIST-ITERATOR does not implement EMPTYP"))

;;
;;    Eliminate CURRENT in favor or NEXT/PREVIOUS? (Both move cursor and return elt. Error if at end?)
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
  ((list :initarg :list)
   (expected-modification-count :type integer))
  (:documentation "External iterator for a mutable list. May traverse in either direction."))

(defmethod initialize-instance :after ((i mutable-list-list-iterator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (list expected-modification-count) i
    (setf expected-modification-count (slot-value list 'modification-count))))

(defmethod count-modification ((i mutable-list-list-iterator))
  (with-slots (expected-modification-count) i
    (incf expected-modification-count)))

(defmethod co-modified ((i mutable-list-list-iterator))
 (with-slots (list expected-modification-count) i
   (/= expected-modification-count (slot-value list 'modification-count))))

(defmethod current :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod current ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement CURRENT"))

(defmethod current-index :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod current-index ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement CURRENT-INDEX"))

(defmethod (setf current) :around (obj (i mutable-list-list-iterator))
  (declare (ignore obj))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod (setf current) (obj (i mutable-list-list-iterator))
  (declare (ignore i obj))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement (SETF CURRENT)"))

(defmethod next :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod next ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement NEXT"))

(defmethod previous :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod previous ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement PREVIOUS"))

(defmethod has-next :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-next ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement HAS-NEXT"))

(defmethod has-previous :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of list")
      (call-next-method)))
(defmethod has-previous ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement HAS-PREVIOUS"))

(defmethod remove :around ((i mutable-list-list-iterator))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod remove ((i mutable-list-list-iterator))
  (declare (ignore i))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement REMOVE"))
(defmethod remove :after ((i mutable-list-list-iterator))
  (count-modification i))

(defmethod add-before :around ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
(defmethod add-before ((i mutable-list-list-iterator) obj)
  (declare (ignore i obj))
  (error "MUTABLE-LIST-LIST-ITERATOR does not implement ADD-BEFORE"))
(defmethod add-before :after ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (count-modification i))

(defmethod add-after :around ((i mutable-list-list-iterator) obj)
  (declare (ignore obj))
  (if (co-modified i)
      (error "List iterator invalid due to structural modification of collection")
      (call-next-method)))
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

(defmethod type ((i random-access-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i random-access-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

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

;;;
;;;    ???
;;;    ITERATOR - initialize from :LIST
;;;    LIST-ITERATOR - initialize from slot LIST
;;;    
(defmethod initialize-instance :after ((i singly-linked-list-list-iterator) &rest initargs &key (start 0))
  (declare (ignore initargs))
  (with-slots (list index cursor) i
    (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
    (initialize-cursor i)
    (loop repeat start do (next i)))) ; Build initial history

;;;
;;;    CURSOR may be detached when:
;;;    1. List iterator is created on empty list
;;;    2. List becomes empty
;;;    
(defun initialize-cursor (list-iterator)
  (with-slots (list cursor) list-iterator
;    (setf cursor (slot-value list 'store))))
    (setf cursor (store list))))
  
(defmethod type ((i singly-linked-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i singly-linked-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

(defmethod current :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod current ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod current-index :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod current-index ((i singly-linked-list-list-iterator))
  (slot-value i 'index))

(defmethod (setf current) :before (obj (i singly-linked-list-list-iterator))
  (declare (ignore obj))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod (setf current) (obj (i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (setf (first cursor) obj)))

;;;
;;;    CURSOR is NIL but STORE is not?? (List iterator created with empty list?)
;;;    
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

(defmethod has-next :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod has-next ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (null (rest cursor)))) )

(defmethod has-previous :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod has-previous ((i singly-linked-list-list-iterator))
  (with-slots (list cursor) i
;    (not (eq cursor (slot-value list 'store)))) )
    (not (eq cursor (store list)))) )

(defmethod remove :before ((i singly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod remove ((i singly-linked-list-list-iterator))
  (with-slots (list cursor index history) i
    (cond ((zerop index)
           (prog1 (delete-node list cursor)
             (initialize-cursor i)))
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

(defmethod (setf current) :before (obj (i doubly-linked-list-list-iterator))
  (declare (ignore obj))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod (setf current) (obj (i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (with-slots (node) cursor
      (setf (content node) obj))))

(defmethod next :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod next ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-next i)
           (advance cursor)
           (current i))
          (t nil))))

(defmethod previous :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod previous ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (cond ((has-previous i)
           (rewind cursor)
           (current i))
          (t nil))))

(defmethod has-next :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod has-next ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-end-p cursor))))

(defmethod has-previous :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (unless (initializedp cursor)
      (reset cursor))))
(defmethod has-previous ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (not (at-start-p cursor))))

(defmethod remove :before ((i doubly-linked-list-list-iterator))
  (with-slots (cursor) i
    (when (null cursor)
      (initialize-cursor i))))
(defmethod remove ((i doubly-linked-list-list-iterator))
  (with-slots (list cursor) i
    (with-slots (index node) cursor
      (cond ((zerop index)
             (prog1 (delete-node list node)
               (reset cursor)))
            (t (let ((current-node node))
                 (cond ((has-next i) (setf node (next node))) ; Half advance...
                       (t (rewind cursor)))
                 (delete-node list current-node)))) )))

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

;;;
;;;    PERSISTENT-LIST-LIST-ITERATOR
;;;    - Must be able to retrieve associated list after structural modifications...
;;;
(defclass persistent-list-list-iterator (list-iterator)
;  ((list :initarg :list)
  ((list :initarg :list :reader list)
   (index :type integer :initarg :index :initform 0)
   (cursor :initarg :cursor :initform '() :type (or null cons))
   (history :initarg :history :initform (make-instance 'persistent-stack :type 'cons))))

;; (defmethod initialize-instance :after ((i persistent-list-list-iterator) &rest initargs &key (start 0))
;;   (declare (ignore initargs))
;;   (with-slots (list index cursor) i
;;     (assert (typep start `(integer 0 (,(max (size list) 1)))) () "Invalid index: ~D" start)
;;     (
    ;; (when (null cursor)
    ;;   (initialize-persistent-cursor i))))
;    (loop repeat start do (next i)))) ; Build initial history

;;;
;;;    Refactor:
;;;    - Make this tail-recursive
;;;    - Improve efficiency of creating list iterator (less garbage)
;;;    - Improve efficiency of creating history stack (less garbage). Provide content directly
;;;    
(defmethod make-instance :around ((class (eql (find-class 'persistent-list-list-iterator))) &rest initargs &key (start 0))
  (cond ((zerop start)
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
  
(defmethod type ((i persistent-list-list-iterator))
  (with-slots (list) i
    (type list)))

(defmethod emptyp ((i persistent-list-list-iterator))
  (with-slots (list) i
    (emptyp list)))

(defmethod current ((i persistent-list-list-iterator))
  (with-slots (cursor) i
    (first cursor)))

(defmethod current-index ((i persistent-list-list-iterator))
  (slot-value i 'index))

;;;
;;;    SETF method need not actually set anything???
;;;    Simply used for value...
;;;    
(defmethod (setf current) (obj (i persistent-list-list-iterator))
  (with-slots (list index) i
    (let ((new-list (setf (nth list index) obj)))
      (make-instance 'persistent-list-list-iterator :list new-list :cursor (slot-value new-list 'store) :start index))))

(defmethod next ((i persistent-list-list-iterator))
  (with-slots (list cursor index history) i
    (cond ((has-next i)
           (make-instance 'persistent-list-list-iterator :list list :cursor (rest cursor) :index (1+ index) :history (push history cursor)))
          (t nil))))  ;<-------------------

(defmethod previous ((i persistent-list-list-iterator))
  (with-slots (list cursor index history) i
    (cond ((has-previous i)
           (make-instance 'persistent-list-list-iterator :list list :cursor (peek history) :index (1- index) :history (pop history)))
          (t nil)))) ;<-------------------

(defmethod has-next ((i persistent-list-list-iterator))
  (with-slots (cursor) i
    (not (null (rest cursor)))) )

(defmethod has-previous ((i persistent-list-list-iterator))
  (with-slots (history) i
    (not (emptyp history))))

(defmethod remove ((i persistent-list-list-iterator))
  ;; (with-slots (list cursor index) i
  ;;   (let ((new-list (delete list index)))
  ;;     (make-instance 'persistent-list-list-iterator
  ;;                    :list new-list
  ;;                    :cursor (slot-value new-list 'store)
  ;;                    :start (cond ((has-next i) index)
  ;;                                 ((has-previous i) (1- index))
  ;;                                 (t 0)))) ))
  (with-slots (list index) i
    (multiple-value-bind (new-list doomed) (delete list index)
      (values (make-instance 'persistent-list-list-iterator
                             :list new-list
                             :cursor (slot-value new-list 'store)
                             :start (cond ((has-next i) index)
                                          ((has-previous i) (1- index))
                                          (t 0)))
              doomed))))

(defmethod add-before ((i persistent-list-list-iterator) obj)
  (with-slots (list index) i
    (cond ((emptyp i) (let ((new-list (add list obj)))
                        (make-instance 'persistent-list-list-iterator
                                       :list new-list
                                       :cursor (slot-value new-list 'store))))
          (t (let ((new-list (insert list index obj)))
               (make-instance 'persistent-list-list-iterator
                              :list new-list
                              :cursor (slot-value new-list 'store)
                              :start (1+ index)))) )))
               
(defmethod add-after ((i persistent-list-list-iterator) obj)
  (with-slots (list index) i
    (cond ((emptyp i) (let ((new-list (add list obj)))
                        (make-instance 'persistent-list-list-iterator
                                       :list new-list
                                       :cursor (slot-value new-list 'store))))
          (t (let ((new-list (insert list (1+ index) obj)))
               (make-instance 'persistent-list-list-iterator
                              :list new-list
                              :cursor (slot-value new-list 'store)
                              :start index)))) ))
