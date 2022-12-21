;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               test-persistent-list.lisp
;;;;
;;;;   Started:            Sat Nov 13 16:02:44 2021
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
;;;;   Notes: Several tests identical to test-list.lisp!
;;;;   - Is TEST-LIST-INSERT(DELETE)-OFFSET meaningful??
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

;; (defmethod fill ((list persistent-list) &key (count 1000) (generator #'identity))
;;   (apply #'add list (loop for i from 1 to count collect i)))

(defun test-persistent-list-constructor (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (assert (null (nth list 0)) () "Accessing element of empty list returns NIL.")
    (handler-case (delete list 0)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DELETE on empty list.~%"))))
  t)

(defun test-persistent-list-emptyp (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (setf list (add list t))
    (assert (not (emptyp list)) () "List with elt should not be empty.")
    (setf list (delete list 0))
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-persistent-list-size (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (loop for i from 1 to count
          do (setf list (add list i))
             (assert (= (size list) i) () "Size of list should be ~D." i))
    (loop for i from (1- count) downto 0
          do (setf list (delete list -1))
             (assert (= (size list) i) () "Size of list should be ~D." i))
    (assert (zerop (size list)) () "Size of empty list should be zero.")
    (loop for i from 1 to count
          do (setf list (insert list 0 i))
             (assert (= (size list) i) () "Size of list should be ~D." i))
    (loop for i from (1- count) downto 0
          do (setf list (delete list 0))
             (assert (= (size list) i) () "Size of list should be ~D." i))
    (assert (zerop (size list)) () "Size of empty list should be zero."))
  t)

(defun test-persistent-list-clear (list-constructor &optional (count 1000))
  (let ((original-list (fill (funcall list-constructor) :count count)))
    (assert (not (emptyp original-list)) () "List should have ~D elements." count)
    (let ((list (clear original-list)))
      (assert (emptyp list) () "List should be empty.")
      (assert (not (emptyp original-list)) () "Original list is unaffected.")
      (assert (not (eq list original-list)) () "Cleared list is new list.")
      (assert (zerop (size list)) () "Size of empty list should be zero.")
      (assert (eq list (clear list)) () "Clearing empty list has no effect.")))
t)

(defun test-persistent-list-elements (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (expected (loop for i from 1 to count collect i))
         (elements (elements list)))
    (assert (equal expected elements) () "FIFO elements should be ~A not ~A" (subseq expected 0 10) (subseq elements 0 10)))
  t)
    
(defun test-persistent-list-contains (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 1 to count
          do (assert (= (contains list i) i) () "The list should contain the value ~D" i)))
  t)

(defun test-persistent-list-contains-predicate (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (assert (every #'(lambda (ch) (contains list ch)) #[#\a #\z])
            ()
            "Should be matchy matchy.")
    (assert (notany #'(lambda (ch) (contains list ch)) #[#\A #\Z])
            ()
            "Default test EQL should fail.")
    (assert (every #'(lambda (ch) (contains list ch :test #'char-equal)) #[#\A #\Z])
            ()
            "Specific test should succeed."))
  t)

(defun test-persistent-list-contains-arithmetic (list-constructor)
  (let ((list (fill (funcall list-constructor) :count 20)))
    (assert (eql (contains list 3) 3) () "Literal 3 should be present in list.")
    (assert (eql (contains list 3d0 :test #'=) 3) () "Integer equal to 3.0 should be present in list.")
    (assert (eql (contains list 3 :test #'(lambda (item elt) (= elt (1+ item)))) 4) ()
            "List contains the element one larger than 3.")
    (assert (eql (contains list 2 :test #'(lambda (item elt) (> elt (* 2 item)))) 5) ()
            "First element in list larger than 2 doubled is 5.")
    (assert (eql (contains list 3 :test #'(lambda (item elt) (zerop (mod elt item)))) 3) ()
            "First multiple of 3 should be present in list."))
  t)

(defun test-persistent-list-equals (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count))
        (persistent-list (fill (make-instance 'persistent-list) :count count))
        (array-list (fill (make-instance 'array-list) :count count))
        (doubly-linked-list (fill (make-instance 'doubly-linked-list) :count count)))
    (assert (equals list list) () "Equality should be reflexive.")
    (assert (equals list persistent-list) () "Lists with same content should be equal.")
    (assert (equals persistent-list list) () "Equality should be symmetric.")
    (assert (equals list array-list) () "Lists with same content should be equal.")
    (assert (equals array-list list) () "Equality should be symmetric.")
    (assert (equals list doubly-linked-list) () "Lists with same content should be equal.")
    (assert (equals doubly-linked-list list) () "Equality should be symmetric."))
  t)

(defun test-persistent-list-equals-predicate (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z]))
        (persistent-list (apply #'add (make-instance 'persistent-list) #[#\A #\Z]))
        (array-list (apply #'add (make-instance 'array-list) #[#\A #\Z]))
        (doubly-linked-list (apply #'add (make-instance 'doubly-linked-list) #[#\A #\Z])))
    (assert (not (equals list persistent-list)) () "Default test EQL should fail.")
    (assert (not (equals list array-list)) () "Default test EQL should fail.")
    (assert (not (equals list doubly-linked-list)) () "Default test EQL should fail.")
    (assert (equals list persistent-list :test #'char-equal) () "Specific test should succeed.")
    (assert (equals list array-list :test #'char-equal) () "Specific test should succeed.")
    (assert (equals list doubly-linked-list :test #'char-equal) () "Specific test should succeed."))
  t)

(defun test-persistent-list-equals-transform (list-constructor)
  (let ((word-list-1 (apply #'add (funcall list-constructor) '("Is" "this" "not" "pung?")))
        (word-list-2 (apply #'add (funcall list-constructor) '("gg" "mcmc" "uuu" "ixncm")))
        (word-list-3 (apply #'add (funcall list-constructor) '("Is" "this" "no" "pung?")))
        (number-list (apply #'add (funcall list-constructor) '(2 4 3 5))))
    (labels ((compare (o1 o2)
               (= (value o1) (value o2)))
             (value (o)
               (etypecase o
                 (string (length o))
                 (number o))))
      (assert (not (equals word-list-1 word-list-2)) () "Default test EQL should fail.")
      (assert (equals word-list-1 word-list-2 :test #'compare) () "Specific test should succeed.")
      (assert (equals word-list-2 word-list-1 :test #'compare) () "Equality should be symmetric.")
      (assert (equals word-list-1 number-list :test #'compare) () "Specific test should succeed.")
      (assert (equals word-list-2 number-list :test #'compare) () "Specific test should succeed.")
      (assert (equals number-list word-list-1 :test #'compare) () "Equality should be symmetric.")
      (assert (not (equals word-list-1 word-list-3 :test #'compare)) () "Unequal lists are not equal.")
      (assert (not (equals number-list word-list-3 :test #'compare)) () "Unequal lists are not equal.")))
  t)

(defun test-persistent-list-each (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (let ((result (with-output-to-string (s)
                    (each list #'(lambda (ch) (write-char ch s)))) )
          (expected (coerce #[#\a #\z] 'string)))
      (assert (string= expected result) () "Writing EACH char should produce ~A: ~A" expected result)))
  t)

(defun test-persistent-list-add (list-constructor &optional (count 1000))
  (loop with list = (funcall list-constructor)
        for i from 1 to count
        do (setf list (add list i))
           (assert (= (size list) i) () "Size of list should be ~D not ~D" i (size list))
           (assert (= (nth list -1) i) () "Last element of list should be ~D not ~D" i (nth list -1)))
  t)

(defun test-persistent-list-insert (list-constructor &key (fill-elt nil))
  (let ((list (funcall list-constructor :fill-elt fill-elt))
        (count 6)
        (elt1 :foo)
        (elt2 :bar))
    (setf list (insert list (1- count) elt1))
    (assert (= (size list) count) () "Insert should extend list.")
    (assert (eq (nth list (1- count)) elt1) () "Inserted element should be ~S." elt1)
    (assert (eq (nth list 0) fill-elt) () "Empty elements should be filled with ~A." fill-elt)
    (setf list (insert list 0 elt2))
    (assert (= (size list) (1+ count)) () "Insert should increase length.")
    (assert (eq (nth list 0) elt2) () "Inserted element should be ~S." elt2))
  t)

(defun test-persistent-list-insert-fill-zero (list-constructor)
  (test-persistent-list-insert list-constructor :fill-elt 0))

(defun test-persistent-list-insert-negative-index (list-constructor)
  (let ((list (add (funcall list-constructor) 0)))
    (loop for i from 1 to 10
          do (setf list (insert list (- i) i)))
    ;; (let ((elts (loop for i below (size list) collect (nth list i)))
    ;;       (expected (loop for i from 10 downto 0 collect i)))
    ;;   (assert (equal elts expected) () "Inserted elements should be: ~A but found: ~A" expected elts)))
    (loop for i from 10 downto 0
          for iterator = (iterator list) then (next iterator)
          do (assert (= i (current iterator)) () "Inserted element should be: ~A but found: ~A" i (current iterator))))
  t)

(defun test-persistent-list-insert-end (list-constructor)
  (let ((list (add (funcall list-constructor) 0 1 2))
        (x 3)
        (y 10))
    (setf list (insert list x x))
    (assert (= (nth list x) x) () "Element at index ~D should be ~D" x x)
    (assert (= (size list) (1+ x)) () "Size of list should be ~D not ~D" (1+ x) (size list))
    (setf list (insert list y y))
    (assert (= (nth list y) y) () "Element at index ~D should be ~D" y y)
    (assert (= (size list) (1+ y)) () "Size of list should be ~D not ~D" (1+ y) (size list)))
  t)

;; ;;;
;; ;;;    Special case for implementations that use an offset (e.g., ARRAY-LIST-X, HASH-TABLE-LIST-X)
;; ;;;    
;; (defun test-list-insert-offset (list-constructor &optional (count 1000))
;;   (let ((low-index 1)
;;         (high-index (* 3/4 count))
;;         (elt 88))
;;     (let ((list (fill (funcall list-constructor) :count count)))
;;       (delete list 0)
;;       (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
;;       (insert list 0 elt)
;;       (assert (= (nth list 0) elt) () "First element should be ~D not ~D." elt (nth list 0)))
;;     (let ((list (fill (funcall list-constructor) :count count)))
;;       (delete list 0)
;;       (insert list low-index elt)
;;       (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
;;       (assert (= (nth list low-index) elt) () "~:R element should be ~D not ~D." (1+ low-index) elt (nth list low-index)))
;;     (let ((list (fill (funcall list-constructor) :count count)))
;;       (delete list 0)
;;       (insert list high-index elt)
;;       (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
;;       (assert (= (nth list high-index) elt) () "~:R element should be ~D not ~D." (1+ high-index) elt (nth list high-index))))
;;   t)

(defun test-persistent-list-delete (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat (size list)
          for expected = (nth list 0)
          do (multiple-value-bind (l doomed) (delete list 0)
               (setf list l)
               (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected)))
    (assert (emptyp list) () "Empty list should be empty."))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from (1- count) downto 0
          for expected = (nth list i)
          do (multiple-value-bind (l doomed) (delete list i)
               (setf list l)
               (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected)))
    (assert (emptyp list) () "Empty list should be empty."))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat (size list)
          for expected = (nth list -1)
          do (multiple-value-bind (l doomed) (delete list -1)
               (setf list l)
               (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected)))
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-persistent-list-delete-random (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat count
          for i = (random (size list))
          for expected = (nth list i)
          do (multiple-value-bind (l doomed) (delete list i)
               (setf list l)
               (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected)))
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-persistent-list-nth (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 0 below count
          do (assert (= (nth list i) (1+ i)) () "~:R element should be: ~A" i (1+ i))))
  t)

(defun test-persistent-list-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from -1 downto (- count)
          do (assert (= (nth list i) (+ count i 1)) () "~:R element should be: ~D not ~D" i (+ count i 1) (nth list i))))
  t)

(defun test-persistent-list-setf-nth (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (loop for i from 0 to count
          do (assert (= (size list) i) () "Prior to SETF size should be ~D not ~D" i (size list))
             (setf list (setf (nth list i) i))
             (assert (= (size list) (1+ i)) () "After SETF size should be ~D not ~D" (1+ i) (size list)))
    (loop for i from 0 to count
          do (assert (= (nth list i) i) () "Element ~D should have value ~D not ~D" i i (nth list i))))
  t)

(defun test-persistent-list-setf-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from -1 downto (- count)
          do (setf list (setf (nth list i) i)))
    (loop for i from 0 below count
          do (assert (= (nth list i) (- i count)) () "Element ~D should have value ~D not ~D" i (- i count) (nth list i))))
  t)

(defun test-persistent-list-setf-nth-out-of-bounds (list-constructor)
  (let ((list (funcall list-constructor))
        (index 10)
        (elt :foo))
    (setf list (setf (nth list index) elt))
    (assert (eq (nth list 0) (fill-elt list)) () "Empty elements should be filled with ~S." (fill-elt list))
    (assert (= (size list) (1+ index)) () "List should expand to accommodate out-of-bounds index.")
    (assert (eq (nth list index) elt) () "~:R element should be: ~A" index elt))
  t)

(defun test-persistent-list-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 1 to count
          do (assert (= (index list i) (1- i)) () "The value ~D should be located at index ~D" (1- i) i)))
  t)

(defun test-persistent-list-index-predicate (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (assert (notany #'(lambda (ch) (index list ch)) #[#\A #\Z])
            ()
            "Default test EQL should fail.")
    (assert (every #'(lambda (ch) (index list ch :test #'char-equal)) #[#\A #\Z])
            ()
            "Specific test should succeed."))
  t)

(defun test-persistent-list-index-arithmetic (list-constructor)
  (let ((list (fill (funcall list-constructor) :count 20)))
    (assert (= (index list 3) 2) () "Literal 3 should be at index 2.")
    (assert (= (index list 3d0 :test #'=) 2) () "Integer equal to 3.0 should be present in list at index 2.")
    (assert (= (index list 3 :test #'(lambda (item elt) (= elt (1+ item)))) 3) ()
            "List contains the element one larger than 3 at index 3.")
    (assert (= (index list 2 :test #'(lambda (item elt) (> elt (* 2 item)))) 4) ()
            "First element in list larger than 2 doubled is 5 at index 4.")
    (assert (= (index list 3 :test #'(lambda (item elt) (zerop (mod elt item)))) 2) ()
            "First multiple of 3 should be at index 2."))
  t)
    
(defun test-persistent-list-slice (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (j (floor count 10))
         (n (floor count 2))
         (slice (slice list j n)))
    (assert (= (size slice) n) () "Slice should contain ~D elements" n)
    (loop for i below n
          do (assert (= (nth slice i) (nth list (+ i j))) () "Element ~D should have value ~D not ~D" i (nth list (+ i j)) (nth slice i))))
  t)

(defun test-persistent-list-slice-negative-index (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (j (floor count 2))
         (n (floor count 2))
         (slice (slice list (- j))))
    (assert (= (size slice) n) () "Slice should contain ~D elements" n)
    (loop for i below n
          do (assert (= (nth slice i) (nth list (+ i j))) () "Element ~D should have value ~D not ~D" i (nth list (+ i j)) (nth slice i))))
  t)

(defun test-persistent-list-slice-corner-cases (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count))
        (n 10))
    (let ((slice (slice list (size list) n)))
      (assert (emptyp slice) () "Slice at end of list should be empty"))
    (let ((slice (slice list (- n) n)))
      (assert (= n (size slice)) () "Slice of last ~D elements should have ~D elements: ~D" n n (size slice)))
    (let ((slice (slice list (- (1+ count)) n)))
      (assert (emptyp slice) () "Slice with invalid negative index should be empty")))
  t)

(defun test-persistent-list-reverse (list-constructor &optional (count 1000))
  (let* ((original (fill (funcall list-constructor) :count count))
         (expected (fill (funcall list-constructor) :count count :generator #'(lambda (i) (1+ (- count i)))) )
         (backward (reverse original))
         (forward (reverse backward)))
    (assert (equals expected backward) () "Reversed list should be: ~A instead of: ~A~%" (slice expected 0 20) (slice backward 0 20))
    (assert (equals original forward) () "Reversed reversed list should be: ~A instead of: ~A~%" (slice original 0 20) (slice forward 0 20)))
  t)

(defun test-persistent-list-time (list-constructor)
  (let ((list (funcall list-constructor)))
    (format t "Add to front of list.~%")
    (time (dotimes (i 10 t)
            (dotimes (j 10000)
              (setf list (insert list 0 j)))
            (setf list (clear list)))) )
  (let ((list (funcall list-constructor)))
    (format t "Add to end of list.~%")
    (time (dotimes (i 10 t)
            (dotimes (j 10000)
              (setf list (add list j)))
            (setf list (clear list)))) )
  (let ((list (funcall list-constructor)))
    (format t "Insert at random index.~%")
    (time (dotimes (i 10 t)
            (setf list (add list :x))
            (dotimes (j 10000)
              (setf list (insert list (random (size list)) j)))
            (setf list (clear list)))) )
  (format t "Delete from front of list.~%")
  (time (dotimes (i 10 t)
          (loop for list = (fill (funcall list-constructor) :count 10000) then (delete list 0)
                until (emptyp list))))
  (format t "Delete from end of list.~%")
  (time (dotimes (i 10 t)
          (loop for list = (fill (funcall list-constructor) :count 10000) then (delete list -1)
                until (emptyp list))))
  (let ((list (funcall list-constructor)))
    (format t "Delete from random index.~%")
    (time (dotimes (i 10 t)
            (setf list (fill list :count 10000))
            (dotimes (j 10000)
              (setf list (delete list (random (size list)))) ))))
  (let ((list (fill (funcall list-constructor) :count 10000)))
    (format t "Sequential access of list elements.~%")
    (time (dotimes (i 10 t)
            (loop for j from 0 below (size list)
                  do (assert (= (nth list j) (1+ j)) () "~:R element should be: ~A" j (1+ j)))) ))
  (let ((list (fill (funcall list-constructor) :count 10000)))
    (format t "Random access of list elements.~%")
    (time (dotimes (i 10 t)
            (loop repeat (size list)
                  for index = (random (size list)) ; RANDOM-STATE??
                  do (assert (= (nth list index) (1+ index)) () "~:R element should be: ~A" index (1+ index)))) ))
  t)

(deftest persistent-list-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-persistent-list-constructor
                 test-persistent-list-emptyp
                 test-persistent-list-size
                 test-persistent-list-clear
                 test-persistent-list-elements
                 test-persistent-list-contains
                 test-persistent-list-contains-predicate
                 test-persistent-list-contains-arithmetic
                 test-persistent-list-equals
                 test-persistent-list-equals-predicate
                 test-persistent-list-equals-transform
                 test-persistent-list-each
                 test-persistent-list-add
                 test-persistent-list-insert
                 test-persistent-list-insert-fill-zero
                 test-persistent-list-insert-negative-index
                 test-persistent-list-insert-end
;;                 test-persistent-list-insert-offset
                 test-persistent-list-delete
;;                 test-persistent-list-delete-offset
                 test-persistent-list-delete-random
                 test-persistent-list-nth
                 test-persistent-list-nth-negative-index
                 test-persistent-list-setf-nth
                 test-persistent-list-setf-nth-negative-index
                 test-persistent-list-setf-nth-out-of-bounds
                 test-persistent-list-index
                 test-persistent-list-index-predicate
                 test-persistent-list-index-arithmetic
                 test-persistent-list-slice
                 test-persistent-list-slice-negative-index
                 test-persistent-list-slice-corner-cases
                 test-persistent-list-reverse
                 test-persistent-list-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-persistent-list ()
  (check
   (persistent-list-test-suite #'(lambda (&key fill-elt) (make-instance 'persistent-list :fill-elt fill-elt)))) )
