;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               test-list.lisp
;;;;
;;;;   Started:            Sun Oct 10 11:06:58 2021
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
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

(defun test-list-constructor (list-constructor)
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

(defun test-list-emptyp (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (add list t)
    (assert (not (emptyp list)) () "List with elt should not be empty.")
    (delete list 0)
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-list-size (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (loop for i from 1 to count
          do (add list i)
             (assert-list-size list i))
    (loop for i from (1- count) downto 0
          do (delete list -1)
             (assert-list-size list i))
    (assert (zerop (size list)) () "Size of empty list should be zero.")
    (loop for i from 1 to count
          do (insert list 0 i)
             (assert-list-size list i))
    (loop for i from (1- count) downto 0
          do (delete list 0)
             (assert-list-size list i))
    (assert (zerop (size list)) () "Size of empty list should be zero."))
  t)

(defun assert-list-size (list n)
  (assert (= (size list) n) () "Size of list should be ~D." n))

(defun test-list-clear (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (assert (not (emptyp list)) () "List should have ~D elements." count)
    (clear list)
    (assert (emptyp list) () "List should be empty.")
    (assert (zerop (size list)) () "Size of empty list should be zero."))
  t)

(defun test-list-elements (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (expected (loop for i from 1 to count collect i))
         (elements (elements list)))
    (assert (equal expected elements) () "FIFO elements should be ~A not ~A" (subseq expected 0 10) (subseq elements 0 10))
    (assert (emptyp list) () "Mutable list should be empty after elements are extracted."))
  t)
    
(defun test-list-contains (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 1 to count
          do (assert (= (contains list i) i) () "The list should contain the value ~D" i)))
  t)

(defun test-list-contains-predicate (list-constructor)
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

(defun test-list-contains-arithmetic (list-constructor)
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
    
(defun test-list-equals (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count))
        (array-list (fill (make-instance 'array-list) :count count))
        (doubly-linked-list (fill (make-instance 'doubly-linked-list) :count count)))
    (assert (equals list list) () "Equality should be reflexive.")
    (assert (equals list array-list) () "Lists with same content should be equal.")
    (assert (equals array-list list) () "Equality should be symmetric.")
    (assert (equals list doubly-linked-list) () "Lists with same content should be equal.")
    (assert (equals doubly-linked-list list) () "Equality should be symmetric."))
  t)

(defun test-list-equals-predicate (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z]))
        (array-list (apply #'add (make-instance 'array-list) #[#\A #\Z]))
        (doubly-linked-list (apply #'add (make-instance 'doubly-linked-list) #[#\A #\Z])))
    (assert (not (equals list array-list)) () "Default test EQL should fail.")
    (assert (not (equals list doubly-linked-list)) () "Default test EQL should fail.")
    (assert (equals list array-list :test #'char-equal) () "Specific test should succeed.")
    (assert (equals list doubly-linked-list :test #'char-equal) () "Specific test should succeed."))
  t)

(defun test-list-equals-transform (list-constructor)
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

(defun test-list-each (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    ;; (loop for ch in #[#\a #\z]
    ;;       do (add list ch))
    (let ((result (with-output-to-string (s)
                    (each list #'(lambda (ch) (write-char ch s)))) )
          (expected (coerce #[#\a #\z] 'string)))
      (assert (string= expected result) () "Writing EACH char should produce ~A: ~A" expected result)))
  t)
  
(defun test-list-add (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (loop for i from 1 to count
          do (add list i)
             (assert (= (size list) i) () "Size of list should be ~D not ~D" i (size list))
             (assert (= (nth list -1) i) () "Last element of list should be ~D not ~D" i (nth list -1))))
  t)

(defun test-list-insert (list-constructor &key (fill-elt nil))
  (let ((list (funcall list-constructor :fill-elt fill-elt))
        (count 6)
        (elt1 :foo)
        (elt2 :bar))
    (insert list (1- count) elt1)
    (assert (= (size list) count) () "Insert should extend list.")
    (assert (eq (nth list (1- count)) elt1) () "Inserted element should be ~S." elt1)
    (assert (eq (nth list 0) fill-elt) () "Empty elements should be filled with ~S." fill-elt)
    (insert list 0 elt2)
    (assert (= (size list) (1+ count)) () "Insert should increase length.")
    (assert (eq (nth list 0) elt2) () "Inserted element should be ~S." elt2))
  t)

(defun test-list-insert-fill-zero (list-constructor)
  (test-list-insert list-constructor :fill-elt 0))

(defun test-list-insert-negative-index (list-constructor)
  (let ((list (add (funcall list-constructor) 0)))
    (loop for i from 1 to 10
          do (insert list (- i) i))
    ;; (let ((elts (loop for i below (size list) collect (nth list i)))
    ;;       (expected (loop for i from 10 downto 0 collect i)))
    ;;   (assert (equal elts expected) () "Inserted elements should be: ~A but found: ~A" expected elts)))
    (loop for i from 10 downto 0
          with iterator = (iterator list)
          do (assert (= i (current iterator)) () "Inserted element should be: ~A but found: ~A" i (current iterator))
             (next iterator)))
  t)

(defun test-list-insert-end (list-constructor)
  (let ((list (add (funcall list-constructor) 0 1 2))
        (x 3)
        (y 10))
    (insert list x x)
    (assert (= (nth list x) x) () "Element at index ~D should be ~D" x x)
    (assert (= (size list) (1+ x)) () "Size of list should be ~D not ~D" (1+ x) (size list))
    (insert list y y)
    (assert (= (nth list y) y) () "Element at index ~D should be ~D" y y)
    (assert (= (size list) (1+ y)) () "Size of list should be ~D not ~D" (1+ y) (size list)))
  t)

;;;
;;;    Special case for implementations that use an offset (e.g., ARRAY-LIST-X, HASH-TABLE-LIST-X)
;;;    
(defun test-list-insert-offset (list-constructor &optional (count 1000))
  (let ((low-index 1)
        (high-index (* 3/4 count))
        (elt 88))
    (let ((list (fill (funcall list-constructor) :count count)))
      (delete list 0)
      (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
      (insert list 0 elt)
      (assert (= (nth list 0) elt) () "First element should be ~D not ~D." elt (nth list 0)))
    (let ((list (fill (funcall list-constructor) :count count)))
      (delete list 0)
      (insert list low-index elt)
      (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
      (assert (= (nth list low-index) elt) () "~:R element should be ~D not ~D." (1+ low-index) elt (nth list low-index)))
    (let ((list (fill (funcall list-constructor) :count count)))
      (delete list 0)
      (insert list high-index elt)
      (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
      (assert (= (nth list high-index) elt) () "~:R element should be ~D not ~D." (1+ high-index) elt (nth list high-index))))
  t)

(defun test-list-delete (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat (size list)
          for expected = (nth list 0)
          for doomed = (delete list 0)
          do (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected))
    (assert (emptyp list) () "Empty list should be empty."))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from (1- count) downto 0
          for expected = (nth list i)
          for doomed = (delete list i)
          do (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected))
    (assert (emptyp list) () "Empty list should be empty."))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat (size list)
          for expected = (nth list -1)
          for doomed = (delete list -1)
          do (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected))
    (assert (emptyp list) () "Empty list should be empty."))
  t)

;;;
;;;    Special case for implementations that use an offset (e.g., ARRAY-LIST-X, HASH-TABLE-LIST-X)
;;;    Has to know too many implementation details to be effective?!
;;;    
(defun test-list-delete-offset (list-constructor &optional (count 1000))
  (let ((low-index 1)
        (high-index (* 3/4 count))
        (list (fill (funcall list-constructor) :count count)))
    (delete list 0)
    (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
    (delete list low-index)
    (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
    (assert (= (nth list low-index) (+ low-index 3)) () "~:R element should be ~D not ~D." (1+ low-index) (+ low-index 3) (nth list low-index))
    (delete list high-index)
    (assert (= (nth list 0) 2) () "First element should be ~D not ~D." 2 (nth list 0))
    (assert (= (nth list high-index) (+ high-index 4)) () "~:R element should be ~D not ~D." (1+ high-index) (+ high-index 4) (nth list high-index)))
  t)

(defun test-list-delete-random (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop repeat count
          for i = (random (size list))
          for expected = (nth list i)
          for doomed = (delete list i)
          do (assert (= expected doomed) () "Incorrect deleted value returned: ~D rather than ~D" doomed expected))
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-list-nth (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 0 below count
          do (assert (= (nth list i) (1+ i)) () "~:R element should be: ~A" i (1+ i))))
  t)

(defun test-list-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from -1 downto (- count)
          do (assert (= (nth list i) (+ count i 1)) () "~:R element should be: ~D not ~D" i (+ count i 1) (nth list i))))
  t)

(defun test-list-setf-nth (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (loop for i from 0 to count
          do (assert (= (size list) i) () "Prior to SETF size should be ~D not ~D" i (size list))
             (setf (nth list i) i)
             (assert (= (size list) (1+ i)) () "After SETF size should be ~D not ~D" (1+ i) (size list)))
    (loop for i from 0 to count
          do (assert (= (nth list i) i) () "Element ~D should have value ~D not ~D" i i (nth list i))))
  t)

(defun test-list-setf-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from -1 downto (- count)
          do (setf (nth list i) i))
    (loop for i from 0 below count
          do (assert (= (nth list i) (- i count)) () "Element ~D should have value ~D not ~D" i (- i count) (nth list i))))
  t)

(defun test-list-setf-nth-out-of-bounds (list-constructor)
  (let ((list (funcall list-constructor))
        (index 10)
        (elt :foo))
    (setf (nth list index) elt)
    (assert (eq (nth list 0) (fill-elt list)) () "Empty elements should be filled with ~S." (fill-elt list))
    (assert (= (size list) (1+ index)) () "List should expand to accommodate out-of-bounds index.")
    (assert (eq (nth list index) elt) () "~:R element should be: ~A" index elt))
  t)

(defun test-list-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count)))
    (loop for i from 1 to count
          do (assert (= (index list i) (1- i)) () "The value ~D should be located at index ~D" (1- i) i)))
  t)

(defun test-list-index-predicate (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (assert (notany #'(lambda (ch) (index list ch)) #[#\A #\Z])
            ()
            "Default test EQL should fail.")
    (assert (every #'(lambda (ch) (index list ch :test #'char-equal)) #[#\A #\Z])
            ()
            "Specific test should succeed."))
  t)

(defun test-list-index-arithmetic (list-constructor)
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
    
(defun test-list-slice (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (j (floor count 10))
         (n (floor count 2))
         (slice (slice list j n)))
    (assert (= (size slice) n) () "Slice should contain ~D elements" n)
    (loop for i below n
          do (assert (= (nth slice i) (nth list (+ i j))) () "Element ~D should have value ~D not ~D" i (nth list (+ i j)) (nth slice i))))
  t)

(defun test-list-slice-negative-index (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) :count count))
         (j (floor count 2))
         (n (floor count 2))
         (slice (slice list (- j))))
    (assert (= (size slice) n) () "Slice should contain ~D elements" n)
    (loop for i below n
          do (assert (= (nth slice i) (nth list (+ i j))) () "Element ~D should have value ~D not ~D" i (nth list (+ i j)) (nth slice i))))
  t)

(defun test-list-slice-corner-cases (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) :count count))
        (n 10))
    (let ((slice (slice list (size list) n)))
      (assert (emptyp slice) () "Slice at end of list should be empty"))
    (let ((slice (slice list (- n) n)))
      (assert (= n (size slice)) () "Slice of last ~D elements should have ~D elements: ~D" n n (size slice)))
    (let ((slice (slice list (- (1+ count)) n)))
      (assert (emptyp slice) () "Slice with invalid negative index should be empty")))
  t)

;;;
;;;    Tricky with in-place reverses: DOUBLY-LINKED-LIST-RATCHET, DOUBLY-LINKED-LIST-HASH-TABLE
;;;    
(defun test-list-reverse (list-constructor &optional (count 1000))
  (let* ((original (fill (funcall list-constructor) :count count))
         (backward (reverse original))
         (expected (fill (funcall list-constructor) :count count :generator #'(lambda (i) (1+ (- count i)))) ))
    (assert (equals expected backward) () "Reversed list should be: ~A instead of: ~A~%" (slice expected 0 20) (slice backward 0 20))
    (let ((forward (reverse backward))) ; This has to come after first ASSERT with in-place reverses!
      (assert (equals original forward) () "Reversed reversed list should be: ~A instead of: ~A~%" (slice original 0 20) (slice forward 0 20))))
  t)

;;;
;;;    See timings below.
;;;    
;; (defun test-time (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (time
;;      (progn
;;        (dotimes (i 10 t)
;;          (fill list 10000) ; This is slow for SINGLY-LINKED-LIST!
;;          (loop until (emptyp list) do (delete list 0))) ; This is really slow for HASH-TABLE-LIST
;;        (dotimes (i 10 t)
;;          (fill list 10000)
;;          (loop until (emptyp list) do (delete list -1)))) ))) ; Very fast for HASH-TABLE-LIST/ARRAY-LIST

;;;
;;;    Time:
;;;    - NTH
;;;    - Sequential access
;;;    - Random access
;;;    
(defun test-list-time (list-constructor)
  (let ((list (funcall list-constructor)))
    (format t "Add to front of list.~%")
    (time (dotimes (i 10 t)
            (dotimes (j 10000)
              (insert list 0 j))
            (clear list))))
  (let ((list (funcall list-constructor)))
    (format t "Add to end of list.~%")
    (time (dotimes (i 10 t)
            (dotimes (j 10000)
              (add list j))
            (clear list))))
  (let ((list (funcall list-constructor)))
    (format t "Insert at random index.~%")
    (time (dotimes (i 10 t)
            (add list :x)
            (dotimes (j 10000)
              (insert list (random (size list)) j))
            (clear list))))
  (let ((list (funcall list-constructor)))
    (format t "Delete from front of list.~%")
    (time (dotimes (i 10 t)
            (fill list :count 10000) ; This is slow for SINGLY-LINKED-LIST!
            (loop until (emptyp list) do (delete list 0)))) ) ; This is really slow for HASH-TABLE-LIST
  (let ((list (funcall list-constructor)))
    (format t "Delete from end of list.~%")
    (time (dotimes (i 10 t)
            (fill list :count 10000)
            (loop until (emptyp list) do (delete list -1)))) ) ; Very fast for HASH-TABLE-LIST/ARRAY-LIST
  (let ((list (funcall list-constructor)))
    (format t "Delete from random index.~%")
    (time (dotimes (i 10 t)
            (fill list :count 10000)
            (dotimes (j 10000)
              (delete list (random (size list)))) )))
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


;; (defun test-wave (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (fill list 5000)
;;     (assert (= (size list) 5000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 2000))
;;     (fill list 5000)
;;     (assert (= (size list) 7000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 4000))
;;     (fill list 5000)
;;     (assert (= (size list) 9000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 6000))
;;     (fill list 4000)
;;     (assert (= (size list) 10000))
;;     (dotimes (i 10000)
;;       (pop list))
;;     (assert (emptyp list)))
;;   t)

;;;
;;;    :BACKWARD DOUBLY-LINKED-LIST-RATCHET ???
;;;    
;; (deftest list-test-suite (class)
;;   (format t "Testing ~A~%" (class-name class))
;;   (let ((constructor #'(lambda (&key fill-elt) (make-instance class :fill-elt

(deftest list-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-list-constructor
                 test-list-emptyp
                 test-list-size 
                 test-list-clear
                 test-list-elements
                 test-list-contains
                 test-list-contains-predicate
                 test-list-contains-arithmetic
                 test-list-equals
                 test-list-equals-predicate
                 test-list-equals-transform
                 test-list-each
                 test-list-add
                 test-list-insert
                 test-list-insert-fill-zero
                 test-list-insert-negative-index
                 test-list-insert-end
                 test-list-insert-offset
                 test-list-delete
                 test-list-delete-offset
                 test-list-delete-random
                 test-list-nth
                 test-list-nth-negative-index
                 test-list-setf-nth
                 test-list-setf-nth-negative-index
                 test-list-setf-nth-out-of-bounds
                 test-list-index
                 test-list-index-predicate
                 test-list-index-arithmetic
                 test-list-slice
                 test-list-slice-negative-index
                 test-list-slice-corner-cases
                 test-list-reverse
                 test-list-time)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-array-list ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'array-list :fill-elt fill-elt)))) )

(deftest test-array-list-x ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'array-list-x :fill-elt fill-elt)))) )

(deftest test-singly-linked-list ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'singly-linked-list :fill-elt fill-elt)))) )

(deftest test-singly-linked-list-x ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'singly-linked-list-x :fill-elt fill-elt)))) )

(deftest test-doubly-linked-list ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list :fill-elt fill-elt)))) )

(deftest test-doubly-linked-list-ratchet ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list-ratchet :fill-elt fill-elt)))
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list-ratchet :fill-elt fill-elt :direction :backward)))) )

(deftest test-doubly-linked-list-hash-table ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list-hash-table :fill-elt fill-elt)))) )

(deftest test-hash-table-list ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'hash-table-list :fill-elt fill-elt)))) )

(deftest test-hash-table-list-x ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'hash-table-list-x :fill-elt fill-elt)))) )

(deftest test-hash-table-list-z ()
  (check
   (list-test-suite #'(lambda (&key fill-elt) (make-instance 'hash-table-list-z :fill-elt fill-elt)))) )

(deftest test-list-all ()
  (check
   (test-array-list)
   (test-array-list-x)
   (test-singly-linked-list)
   (test-singly-linked-list-x)
   (test-doubly-linked-list)
   (test-doubly-linked-list-ratchet)
   (test-doubly-linked-list-hash-table)
   (test-hash-table-list)
   (test-hash-table-list-x)
   (test-hash-table-list-z)))

;;;
;;;    SBCL timings.
;;;    
;;;
;;;    Filling a SINGLY-LINKED-LIST is very slow.
;;;    DOUBLY-LINKED-LIST (with single cursor!) performs well overall.
;;;    
;; * (let ((list (make-instance 'array-list))) (time (fill list 100000)))
;; Evaluation took:
;;   0.010 seconds of real time
;;   0.010078 seconds of total run time (0.010077 user, 0.000001 system)
;;   100.00% CPU
;;   36,273,864 processor cycles
;;   5,834,736 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (time (fill list 100000)))
;; Evaluation took:
;;   14.807 seconds of real time
;;   14.806637 seconds of total run time (14.806637 user, 0.000000 system)
;;   [ Run times consist of 0.005 seconds GC time, and 14.802 seconds non-GC time. ]
;;   100.00% CPU
;;   53,306,125,660 processor cycles
;;   6,389,760 bytes consed

;; NIL

;; * (let ((list (make-instance 'doubly-linked-list))) (time (fill list 100000)))
;; Evaluation took:
;;   0.016 seconds of real time
;;   0.016405 seconds of total run time (0.016405 user, 0.000000 system)
;;   100.00% CPU
;;   59,052,320 processor cycles
;;   9,624,464 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (time (fill list 100000)))
;; Evaluation took:
;;   0.035 seconds of real time
;;   0.034909 seconds of total run time (0.034909 user, 0.000000 system)
;;   100.00% CPU
;;   125,682,040 processor cycles
;;   14,652,752 bytes consed
  
;; NIL
;;;
;;;    Much faster to add all at once!!!
;;;
;; * (let ((list (make-instance 'singly-linked-list)) (vals (loop for i from 1 to 100000 collect i))) (time (apply #'add list vals)))
;; Evaluation took:
;;   0.007 seconds of real time
;;   0.007720 seconds of total run time (0.007720 user, 0.000000 system)
;;   114.29% CPU
;;   27,800,808 processor cycles
;;   4,805,632 bytes consed
  
;; NIL

;;;
;;;    Even adding to end of a long list!
;;;
;; * (let ((list (make-instance 'singly-linked-list)) (vals (loop for i from 1 to 100000 collect i))) (time (progn (apply #'add list vals) (apply #'add list vals))))
;; Evaluation took:
;;   0.015 seconds of real time
;;   0.014990 seconds of total run time (0.014990 user, 0.000000 system)
;;   100.00% CPU
;;   54,057,764 processor cycles
;;   9,578,496 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'array-list)) (vals (loop for i from 1 to 100000 collect i))) (time (progn (apply #'add list vals) (apply #'add list vals))))
;; Evaluation took:
;;   0.021 seconds of real time
;;   0.021412 seconds of total run time (0.021412 user, 0.000000 system)
;;   [ Run times consist of 0.005 seconds GC time, and 0.017 seconds non-GC time. ]
;;   100.00% CPU
;;   77,190,946 processor cycles
;;   11,580,816 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'doubly-linked-list)) (vals (loop for i from 1 to 100000 collect i))) (time (progn (apply #'add list vals) (apply #'add list vals))))
;; Evaluation took:
;;   0.020 seconds of real time
;;   0.019754 seconds of total run time (0.019754 user, 0.000000 system)
;;   100.00% CPU
;;   71,206,474 processor cycles
;;   19,212,256 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list)) (vals (loop for i from 1 to 100000 collect i))) (time (progn (apply #'add list vals) (apply #'add list vals))))
;; Evaluation took:
;;   0.033 seconds of real time
;;   0.033554 seconds of total run time (0.029564 user, 0.003990 system)
;;   103.03% CPU
;;   120,936,976 processor cycles
;;   29,216,720 bytes consed
  
;; NIL

;;;
;;;    Different implementations perform better deleting from start vs. end...
;;;    
;; * (let ((list (make-instance 'array-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.134 seconds of real time
;;   0.134664 seconds of total run time (0.134664 user, 0.000000 system)
;;   [ Run times consist of 0.023 seconds GC time, and 0.112 seconds non-GC time. ]
;;   100.75% CPU
;;   484,782,628 processor cycles
;;   399,729,744 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.001 seconds of real time                 <-- As fast as DOUBLY-LINKED-LIST if FILL is not included in timing.
;;   0.000440 seconds of total run time (0.000440 user, 0.000000 system)
;;   0.00% CPU
;;   1,581,808 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'doubly-linked-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.001213 seconds of total run time (0.001213 user, 0.000000 system)
;;   100.00% CPU
;;   4,364,580 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   1.484 seconds of real time
;;   1.484654 seconds of total run time (1.484654 user, 0.000000 system)
;;   100.07% CPU
;;   5,344,877,876 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'array-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.001583 seconds of total run time (0.001583 user, 0.000000 system)
;;   100.00% CPU
;;   5,695,860 processor cycles
;;   163,840 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.147 seconds of real time
;;   0.146859 seconds of total run time (0.146859 user, 0.000000 system)
;;   100.00% CPU
;;   528,689,678 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'doubly-linked-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.007 seconds of real time
;;   0.006955 seconds of total run time (0.006955 user, 0.000000 system)
;;   100.00% CPU
;;   25,035,522 processor cycles
;;   1,932,928 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (fill list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.001526 seconds of total run time (0.001526 user, 0.000000 system)
;;   100.00% CPU
;;   5,490,072 processor cycles
;;   0 bytes consed
  
;; NIL

;;;
;;;    Deleting from front of ARRAY-LIST is the only one that Ruby beats SBCL!
;;;    
;; * (test-time #'(lambda () (make-instance 'array-list)))
;; Evaluation took:
;;   1.368 seconds of real time
;;   1.368592 seconds of total run time (1.364593 user, 0.003999 system)
;;   [ Run times consist of 0.223 seconds GC time, and 1.146 seconds non-GC time. ]
;;   100.07% CPU
;;   4,926,967,636 processor cycles
;;   4,001,007,392 bytes consed
  
;; Evaluation took:
;;   0.027 seconds of real time
;;   0.026421 seconds of total run time (0.026421 user, 0.000000 system)
;;   96.30% CPU
;;   95,110,166 processor cycles
;;   4,816,896 bytes consed
  
;; T
;; * (test-time #'(lambda () (make-instance 'singly-linked-list)))
;; Evaluation took:
;;   1.546 seconds of real time                     <-- This is the cost of FILL!!
;;   1.546163 seconds of total run time (1.546163 user, 0.000000 system)
;;   100.00% CPU
;;   5,566,214,004 processor cycles
;;   6,389,760 bytes consed
  
;; Evaluation took:
;;   3.043 seconds of real time
;;   3.042946 seconds of total run time (3.042946 user, 0.000000 system)
;;   100.00% CPU
;;   10,954,647,808 processor cycles
;;   6,422,528 bytes consed
  
;; T
;; * (test-time #'(lambda () (make-instance 'doubly-linked-list)))
;; Evaluation took:
;;   0.032 seconds of real time
;;   0.032870 seconds of total run time (0.032870 user, 0.000000 system)
;;   [ Run times consist of 0.003 seconds GC time, and 0.030 seconds non-GC time. ]
;;   103.13% CPU
;;   118,326,382 processor cycles
;;   9,591,760 bytes consed
  
;; Evaluation took:
;;   0.075 seconds of real time
;;   0.075748 seconds of total run time (0.075748 user, 0.000000 system)
;;   101.33% CPU
;;   272,684,000 processor cycles
;;   28,790,352 bytes consed
  
;; T
;; * (test-time #'(lambda () (make-instance 'hash-table-list)))
;; Evaluation took:
;;   14.766 seconds of real time
;;   14.766378 seconds of total run time (14.766378 user, 0.000000 system)
;;   100.00% CPU
;;   53,159,234,096 processor cycles
;;   4,231,056 bytes consed
  
;; Evaluation took:
;;   0.025 seconds of real time
;;   0.024374 seconds of total run time (0.024374 user, 0.000000 system)
;;   96.00% CPU
;;   87,741,880 processor cycles
;;   3,178,496 bytes consed
  
;; T
