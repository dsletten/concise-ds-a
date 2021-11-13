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

(defun test-constructor (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (assert (null (nth list 0)) () "Accessing element of empty list returns NIL.")
    t))

(defun test-emptyp (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (add list t)
    (assert (not (emptyp list)) () "List with elt should not be empty.")
    (delete list 0)
    (assert (emptyp list) () "Empty list should be empty.")
    t))

(defun test-size (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (loop for i from 1 to count
          do (add list i)
             (assert-list-size list i)
          finally (return t))))

(defun assert-list-size (list n)
  (assert (= (size list) n) () "Size of list should be ~D." n))

(defun test-clear (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (assert (not (emptyp list)) () "List should have ~D elements." count)
    (clear list)
    (assert (emptyp list) () "List should be empty.")
    t))

(defun fill-list (list count)
  (loop for i from 1 to count
        do (add list i)))

(defun test-contains (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for i from 1 to count
          do (assert (contains list i) () "The list should contain the value ~D" i)))
  t)

(defun test-contains-test (list-constructor)
  (let ((list (funcall list-constructor)))
    (loop for ch in #[#\a #\z]
          do (add list ch))
    (notany #'(lambda (ch) (contains list ch)) #[#\A #\Z])
    (every #'(lambda (ch) (contains list ch :test #'char-equal)) #[#\A #\Z]))
  t)

; test-equals? Different types of list--same elements

(defun test-each (list-constructor)
  (let ((list (funcall list-constructor)))
    (loop for ch in #[#\a #\z]
          do (add list ch))
    (let ((result (with-output-to-string (s)
                    (each list #'(lambda (ch) (write-char ch s)))) )
          (expected (coerce #[#\a #\z] 'string)))
      (assert (string= expected result) () "Writing EACH char should produce ~A: ~A" expected result)))
  t)
  
(defun test-insert (list-constructor &key (fill-elt nil))
  (let ((list (funcall list-constructor :fill-elt fill-elt)))
    (insert list 5 :foo)
    (assert (= (size list) 6) () "Insert should extend list.")
    (assert (eq (nth list 0) fill-elt) () "Empty elements should be filled with ~A." fill-elt)
    (insert list 0 :bar)
    (assert (= (size list) 7) () "Insert should increase length.")
    (assert (eq (nth list 0) :bar) () "Inserted element should be :BAR."))
  t)

(defun test-insert-fill-zero (list-constructor)
  (test-insert list-constructor :fill-elt 0))

(defun test-insert-negative-index (list-constructor)
  (let ((list (funcall list-constructor)))
    (add list 0)
    (loop for i from 1 to 10
          do (insert list (- i) i))
    (let ((elts (loop for i below (size list) collect (nth list i)))
          (expected (loop for i from 10 downto 0 collect i)))
      (assert (equal elts expected) () "Inserted elements should be: ~A but found: ~A" expected elts)))
  t)

(defun test-insert-end (list-constructor)
  (let ((list (funcall list-constructor)))
    (add list 0 1 2)
    (insert list 3 3)
    (assert (= (nth list 3) 3) () "Element at index ~D should be ~D" 3 3)
    (assert (= (size list) 4) () "Size of list should be ~D" 4)
    (insert list 5 5)
    (assert (= (nth list 5) 5) () "Element at index ~D should be ~D" 5 5)
    (assert (= (size list) 6) () "Size of list should be ~D" 6))
  t)

(defun test-delete (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for n from count downto 1
          do (assert (= (size list) n) () "List size should reflect deletions")
             (delete list 0))
;             (unless (emptyp list) (delete list 0))) )
    (assert (emptyp list) () "Empty list should be empty."))
  t)

(defun test-delete-negative-index (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for n from count downto 1
          do (assert (= (delete list -1) n) () "Deleted element should be last in list")))
  t)

(defun test-nth (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for i from 0 below count
          do (assert (= (nth list i) (1+ i)) () "~:R element should be: ~A" i (1+ i))))
  t)

(defun test-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for i from -1 downto (- count)
          do (assert (= (nth list i) (+ count i 1)) () "~:R element should be: ~D not ~D" i (+ count i 1) (nth list i))))
  t)

(defun test-setf-nth (list-constructor)
  (let ((list (funcall list-constructor)))
    (loop for i from 0 to 10
          do (setf (nth list i) i))
    (loop for i from 0 to 10
          do (assert (= (nth list i) i) () "Element ~D should have value ~D not ~D" i i (nth list i))))
  t)

(defun test-setf-nth-negative-index (list-constructor)
  (let ((list (funcall list-constructor)))
    (fill-list list 10)
    (loop for i from -1 downto -10
          do (setf (nth list i) i))
    (loop for i from 0 below 10
          do (assert (= (nth list i) (- i 10)) () "Element ~D should have value ~D not ~D" i (- i 10) (nth list i))))
  t)

(defun test-setf-nth-out-of-bounds (list-constructor)
  (let ((list (funcall list-constructor)))
    (setf (nth list 10) :foo)
    (assert (= (size list) (1+ 10)) () "List should expand to accommodate out-of-bounds index."))
  t)

(defun test-index (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for i from 1 to count
          do (assert (= (index list i) (1- i)) () "The value ~D should be located at index ~D" (1- i) i)))
  t)

(defun test-index-test (list-constructor)
  (let ((list (funcall list-constructor)))
    (loop for ch in #[#\a #\z]
          do (add list ch))
    (notany #'(lambda (ch) (index list ch)) #[#\A #\Z])
    (every #'(lambda (ch) (index list ch :test #'char-equal)) #[#\A #\Z]))
  t)

(defun test-slice (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((slice (slice list 100 500)))
      (assert (= (size slice) 500) () "Slice should contain ~D elements" 500)
      (loop repeat 500
            for i from 0
            do (assert (= (nth slice i) (+ i 100 1)) () "Element ~D should have value ~D not ~D" i (+ i 100 1) (nth slice i)))) )
  t)

(defun test-slice-corner-cases (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((slice (slice list (size list) 10)))
      (assert (emptyp slice) () "Slice at end of list should be empty"))
    (let ((slice (slice list -10 10)))
      (assert (= (size slice) 10) () "Slice of last ~D elements should have ~D elements: ~D" 10 10 (size slice)))
    (let ((slice (slice list -1001 10)))
      (assert (emptyp slice) () "Slice with invalid negative index should be empty")))
  t)

(defun test-time (list-constructor)
  (let ((list (funcall list-constructor)))
    (time
     (progn
       (dotimes (i 10 t)
         (fill-list list 10000)
         (loop until (emptyp list) do (delete list 0)))
       (dotimes (i 10 t)
         (fill-list list 10000)
         (loop until (emptyp list) do (delete list -1)))) )))

;; (defun test-wave (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (fill-list list 5000)
;;     (assert (= (size list) 5000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 2000))
;;     (fill-list list 5000)
;;     (assert (= (size list) 7000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 4000))
;;     (fill-list list 5000)
;;     (assert (= (size list) 9000))
;;     (dotimes (i 3000)
;;       (pop list))
;;     (assert (= (size list) 6000))
;;     (fill-list list 4000)
;;     (assert (= (size list) 10000))
;;     (dotimes (i 10000)
;;       (pop list))
;;     (assert (emptyp list)))
;;   t)

(deftest test-array-list ()
  (check
   (test-constructor #'(lambda () (make-instance 'array-list)))
   (test-emptyp #'(lambda () (make-instance 'array-list)))
   (test-size #'(lambda () (make-instance 'array-list)))
   (test-clear #'(lambda () (make-instance 'array-list)))
   (test-each #'(lambda () (make-instance 'array-list)))
   (test-contains #'(lambda () (make-instance 'array-list)))
   (test-contains-test #'(lambda () (make-instance 'array-list)))
   (test-insert #'(lambda (&key fill-elt) (make-instance 'array-list :fill-elt fill-elt)))
   (test-insert-fill-zero #'(lambda (&key fill-elt) (make-instance 'array-list :fill-elt fill-elt)))
   (test-insert-negative-index #'(lambda () (make-instance 'array-list)))
   (test-insert-end #'make-array-list)
   (test-delete #'(lambda () (make-instance 'array-list)))
   (test-delete-negative-index #'(lambda () (make-instance 'array-list)))
   (test-nth #'(lambda () (make-instance 'array-list)))
   (test-nth-negative-index #'(lambda () (make-instance 'array-list)))
   (test-setf-nth #'(lambda () (make-instance 'array-list)))
   (test-setf-nth-out-of-bounds #'(lambda () (make-instance 'array-list)))
   (test-setf-nth-negative-index #'(lambda () (make-instance 'array-list)))
   (test-index #'(lambda () (make-instance 'array-list)))
   (test-index-test #'(lambda () (make-instance 'array-list)))
   (test-slice #'(lambda () (make-instance 'array-list)))
   (test-slice-corner-cases #'(lambda () (make-instance 'array-list)))

;;    (test-peek #'(lambda () (make-instance 'array-list)))
   (test-time #'(lambda () (make-instance 'array-list)))
;;    (test-wave #'(lambda () (make-instance 'array-list)))) )
))

(deftest test-singly-linked-list ()
  (check
   (test-constructor #'(lambda () (make-instance 'singly-linked-list)))
   (test-emptyp #'(lambda () (make-instance 'singly-linked-list)))
   (test-size #'(lambda () (make-instance 'singly-linked-list)))
   (test-clear #'(lambda () (make-instance 'singly-linked-list)))
   (test-each #'(lambda () (make-instance 'singly-linked-list)))
   (test-contains #'(lambda () (make-instance 'singly-linked-list)))
   (test-contains-test #'(lambda () (make-instance 'singly-linked-list)))
   (test-insert #'(lambda (&key fill-elt) (make-instance 'singly-linked-list :fill-elt fill-elt)))
   (test-insert-fill-zero #'(lambda (&key fill-elt) (make-instance 'singly-linked-list :fill-elt fill-elt)))
   (test-insert-negative-index #'(lambda () (make-instance 'singly-linked-list)))
   (test-insert-end #'make-linked-list)
   (test-delete #'(lambda () (make-instance 'singly-linked-list)))
   (test-delete-negative-index #'(lambda () (make-instance 'singly-linked-list)))
   (test-nth #'(lambda () (make-instance 'singly-linked-list)))
   (test-nth-negative-index #'(lambda () (make-instance 'singly-linked-list)))
   (test-setf-nth #'(lambda () (make-instance 'singly-linked-list)))
   (test-setf-nth-out-of-bounds #'(lambda () (make-instance 'singly-linked-list)))
   (test-setf-nth-negative-index #'(lambda () (make-instance 'singly-linked-list)))
   (test-index #'(lambda () (make-instance 'singly-linked-list)))
   (test-index-test #'(lambda () (make-instance 'singly-linked-list)))
   (test-slice #'make-linked-list)
   (test-slice-corner-cases #'make-linked-list)

   (test-time #'make-linked-list)
;;    (test-wave #'(lambda () (make-instance 'singly-linked-list)))) )
))

(deftest test-doubly-linked-list ()
  (check
   (test-constructor #'(lambda () (make-instance 'doubly-linked-list)))
   (test-emptyp #'(lambda () (make-instance 'doubly-linked-list)))
   (test-size #'(lambda () (make-instance 'doubly-linked-list)))
   (test-clear #'(lambda () (make-instance 'doubly-linked-list)))
   (test-each #'(lambda () (make-instance 'doubly-linked-list)))
   (test-contains #'(lambda () (make-instance 'doubly-linked-list)))
   (test-contains-test #'(lambda () (make-instance 'doubly-linked-list)))
   (test-insert #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list :fill-elt fill-elt)))
   (test-insert-fill-zero #'(lambda (&key fill-elt) (make-instance 'doubly-linked-list :fill-elt fill-elt)))
   (test-insert-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-insert-end #'make-linked-list)
   (test-delete #'(lambda () (make-instance 'doubly-linked-list)))
   (test-delete-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-nth #'(lambda () (make-instance 'doubly-linked-list)))
   (test-nth-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth-out-of-bounds #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-index-test #'(lambda () (make-instance 'doubly-linked-list)))
   (test-slice #'make-linked-list)
   (test-slice-corner-cases #'make-linked-list)

   (test-time #'make-linked-list)
;;    (test-wave #'(lambda () (make-instance 'doubly-linked-list)))) )
))

(deftest test-hash-table-list ()
  (check
   (test-constructor #'(lambda () (make-instance 'hash-table-list)))
   (test-emptyp #'(lambda () (make-instance 'hash-table-list)))
   (test-size #'(lambda () (make-instance 'hash-table-list)))
   (test-clear #'(lambda () (make-instance 'hash-table-list)))
   (test-each #'(lambda () (make-instance 'hash-table-list)))
   (test-contains #'(lambda () (make-instance 'hash-table-list)))
   (test-contains-test #'(lambda () (make-instance 'hash-table-list)))
   (test-insert #'(lambda (&key fill-elt) (make-instance 'hash-table-list :fill-elt fill-elt)))
   (test-insert-fill-zero #'(lambda (&key fill-elt) (make-instance 'hash-table-list :fill-elt fill-elt)))
   (test-insert-negative-index #'(lambda () (make-instance 'hash-table-list)))
   (test-insert-end #'(lambda () (make-instance 'hash-table-list)))
   (test-delete #'(lambda () (make-instance 'hash-table-list)))
   (test-delete-negative-index #'(lambda () (make-instance 'hash-table-list)))
   (test-nth #'(lambda () (make-instance 'hash-table-list)))
   (test-nth-negative-index #'(lambda () (make-instance 'hash-table-list)))
   (test-setf-nth #'(lambda () (make-instance 'hash-table-list)))
   (test-setf-nth-out-of-bounds #'(lambda () (make-instance 'hash-table-list)))
   (test-setf-nth-negative-index #'(lambda () (make-instance 'hash-table-list)))
   (test-index #'(lambda () (make-instance 'hash-table-list)))
   (test-index-test #'(lambda () (make-instance 'hash-table-list)))
   (test-slice #'(lambda () (make-instance 'hash-table-list)))
   (test-slice-corner-cases #'(lambda () (make-instance 'hash-table-list)))

;;    (test-peek #'(lambda () (make-instance 'hash-table-list)))
   (test-time #'(lambda () (make-instance 'hash-table-list)))
;;    (test-wave #'(lambda () (make-instance 'hash-table-list)))) )
))
