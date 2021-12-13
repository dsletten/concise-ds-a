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
          do (assert (= (contains list i) i) () "The list should contain the value ~D" i)))
  t)

(defun test-contains-test (list-constructor)
  (let ((list (funcall list-constructor)))
    (apply #'add list #[#\a #\z])
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
          do (assert (= (delete list -1) n) () "Deleted element should be last in list"))
    (assert (emptyp list) () "Empty list should be empty."))
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

(defun test-setf-nth (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (loop for i from 0 to count
          do (setf (nth list i) i))
    (loop for i from 0 to count
          do (assert (= (nth list i) i) () "Element ~D should have value ~D not ~D" i i (nth list i))))
  t)

(defun test-setf-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (loop for i from -1 downto (- count)
          do (setf (nth list i) i))
    (loop for i from 0 below count
          do (assert (= (nth list i) (- i count)) () "Element ~D should have value ~D not ~D" i (- i count) (nth list i))))
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
    (let* ((n (floor count 2))
           (j (floor count 10))
           (slice (slice list j n)))
      (assert (= (size slice) n) () "Slice should contain ~D elements" n)
      (loop repeat n
            for i from 0
            do (assert (= (nth slice i) (+ i j 1)) () "Element ~D should have value ~D not ~D" i (+ i j 1) (nth slice i)))) )
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

;;;
;;;    See timings below.
;;;    
;; (defun test-time (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (time
;;      (progn
;;        (dotimes (i 10 t)
;;          (fill-list list 10000) ; This is slow for SINGLY-LINKED-LIST!
;;          (loop until (emptyp list) do (delete list 0))) ; This is really slow for HASH-TABLE-LIST
;;        (dotimes (i 10 t)
;;          (fill-list list 10000)
;;          (loop until (emptyp list) do (delete list -1)))) ))) ; Very fast for HASH-TABLE-LIST/ARRAY-LIST

(defun test-time (list-constructor)
  (let ((list (funcall list-constructor)))
    (time (dotimes (i 10 t)
            (fill-list list 10000) ; This is slow for SINGLY-LINKED-LIST!
            (loop until (emptyp list) do (delete list 0)))) ; This is really slow for HASH-TABLE-LIST
    (time (dotimes (i 10 t)
            (fill-list list 10000)
            (loop until (emptyp list) do (delete list -1)))) )) ; Very fast for HASH-TABLE-LIST/ARRAY-LIST

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
   (test-insert-end #'make-doubly-linked-list)
   (test-delete #'(lambda () (make-instance 'doubly-linked-list)))
   (test-delete-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-nth #'(lambda () (make-instance 'doubly-linked-list)))
   (test-nth-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth-out-of-bounds #'(lambda () (make-instance 'doubly-linked-list)))
   (test-setf-nth-negative-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-index #'(lambda () (make-instance 'doubly-linked-list)))
   (test-index-test #'(lambda () (make-instance 'doubly-linked-list)))
   (test-slice #'make-doubly-linked-list)
   (test-slice-corner-cases #'make-doubly-linked-list)

   (test-time #'make-doubly-linked-list)
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

;;;
;;;    SBCL timings.
;;;    
;;;
;;;    Filling a SINGLY-LINKED-LIST is very slow.
;;;    DOUBLY-LINKED-LIST (with single cursor!) performs well overall.
;;;    
;; * (let ((list (make-instance 'array-list))) (time (fill-list list 100000)))
;; Evaluation took:
;;   0.010 seconds of real time
;;   0.010078 seconds of total run time (0.010077 user, 0.000001 system)
;;   100.00% CPU
;;   36,273,864 processor cycles
;;   5,834,736 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (time (fill-list list 100000)))
;; Evaluation took:
;;   14.807 seconds of real time
;;   14.806637 seconds of total run time (14.806637 user, 0.000000 system)
;;   [ Run times consist of 0.005 seconds GC time, and 14.802 seconds non-GC time. ]
;;   100.00% CPU
;;   53,306,125,660 processor cycles
;;   6,389,760 bytes consed

;; NIL

;; * (let ((list (make-instance 'doubly-linked-list))) (time (fill-list list 100000)))
;; Evaluation took:
;;   0.016 seconds of real time
;;   0.016405 seconds of total run time (0.016405 user, 0.000000 system)
;;   100.00% CPU
;;   59,052,320 processor cycles
;;   9,624,464 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (time (fill-list list 100000)))
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
;; * (let ((list (make-instance 'array-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.134 seconds of real time
;;   0.134664 seconds of total run time (0.134664 user, 0.000000 system)
;;   [ Run times consist of 0.023 seconds GC time, and 0.112 seconds non-GC time. ]
;;   100.75% CPU
;;   484,782,628 processor cycles
;;   399,729,744 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.001 seconds of real time                 <-- As fast as DOUBLY-LINKED-LIST if FILL-LIST is not included in timing.
;;   0.000440 seconds of total run time (0.000440 user, 0.000000 system)
;;   0.00% CPU
;;   1,581,808 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'doubly-linked-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   0.001 seconds of real time
;;   0.001213 seconds of total run time (0.001213 user, 0.000000 system)
;;   100.00% CPU
;;   4,364,580 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list 0))))
;; Evaluation took:
;;   1.484 seconds of real time
;;   1.484654 seconds of total run time (1.484654 user, 0.000000 system)
;;   100.07% CPU
;;   5,344,877,876 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'array-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.002 seconds of real time
;;   0.001583 seconds of total run time (0.001583 user, 0.000000 system)
;;   100.00% CPU
;;   5,695,860 processor cycles
;;   163,840 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'singly-linked-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.147 seconds of real time
;;   0.146859 seconds of total run time (0.146859 user, 0.000000 system)
;;   100.00% CPU
;;   528,689,678 processor cycles
;;   0 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'doubly-linked-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list -1))))
;; Evaluation took:
;;   0.007 seconds of real time
;;   0.006955 seconds of total run time (0.006955 user, 0.000000 system)
;;   100.00% CPU
;;   25,035,522 processor cycles
;;   1,932,928 bytes consed
  
;; NIL
;; * (let ((list (make-instance 'hash-table-list))) (fill-list list 10000) (time (loop until (emptyp list) do (delete list -1))))
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
;;   1.546 seconds of real time                     <-- This is the cost of FILL-LIST!!
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
