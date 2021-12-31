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
;;;;   Notes:
;;;;
;;;;
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

(defmethod fill ((list persistent-list) &optional (count 1000))
  (loop for i from 1 to count
        for new-list = (add list i) then (add new-list i)   ; ??????? Scope problem without renaming?!??!
        finally (return new-list)))

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
    (assert (not (emptyp (add list t))) () "List with elt should not be empty.")
    (assert (emptyp (delete (add list t) 0)) () "Empty list should be empty."))
  t)

(defun test-persistent-list-size (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (let ((list (loop for i from 0 to count
                      for new-list = list then (add new-list i)
                      do (assert (= (size new-list) i) () "Size of list should be ~D." i)
                      finally (return new-list))))
      (let ((list (loop for i from count downto 0 ; Same as TEST-PERSISTENT-LIST-DELETE below
                        for new-list = list then (delete new-list 0)
                        do (assert (= (size new-list) i) () "Size of list should be ~D." i)
                        finally (return new-list))))
        (assert (zerop (size list)) () "Size of new list should be zero."))))
  t)

(defun test-persistent-list-clear (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (assert (not (emptyp list)) () "List should have ~D elements." count)
    (assert (emptyp (clear list)) () "List should be empty."))
  t)


(defun test-persistent-list-contains (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (loop for i from 1 to count
          do (assert (= (contains list i) i) () "The list should contain the value ~D" i)))
  t)

(defun test-persistent-list-contains-test (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (assert (notany #'(lambda (ch) (contains list ch)) #[#\A #\Z])
            ()
            "Default test EQL should fail.")
    (assert (every #'(lambda (ch) (contains list ch :test #'char-equal)) #[#\A #\Z])
            ()
            "Specific test should succeed."))
  t)

(defun test-persistent-list-each (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (let ((result (with-output-to-string (s)
                    (each list #'(lambda (ch) (write-char ch s)))) )
          (expected (coerce #[#\a #\z] 'string)))
      (assert (string= expected result) () "Writing EACH char should produce ~A: ~A" expected result)))
  t)

(defun test-persistent-list-equals (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count))
        (persistent-list (fill (make-instance 'persistent-list) count))
        (array-list (fill (make-instance 'array-list) count))
        (doubly-linked-list (fill (make-instance 'doubly-linked-list) count)))
    (assert (equals list persistent-list) () "Lists with same content should be equal.")
    (assert (equals persistent-list list) () "Equality should be commutative.")
    (assert (equals list array-list) () "Lists with same content should be equal.")
    (assert (equals array-list list) () "Equality should be commutative.")
    (assert (equals list doubly-linked-list) () "Lists with same content should be equal.")
    (assert (equals doubly-linked-list list) () "Equality should be commutative."))
  t)

(defun test-persistent-list-equals-test (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z]))
        (persistent-list (apply #'add (make-instance 'persistent-list) #[#\A #\Z]))
        (array-list (make-instance 'array-list))
        (doubly-linked-list (make-instance 'doubly-linked-list)))
    (apply #'add array-list #[#\A #\Z])
    (apply #'add doubly-linked-list #[#\A #\Z])
    (assert (not (equals list persistent-list)) () "Default test EQL should fail.")
    (assert (not (equals list array-list)) () "Default test EQL should fail.")
    (assert (not (equals list doubly-linked-list)) () "Default test EQL should fail.")
    (assert (equals list persistent-list :test #'char-equal) () "Specific test should succeed.")
    (assert (equals list array-list :test #'char-equal) () "Specific test should succeed.")
    (assert (equals list doubly-linked-list :test #'char-equal) () "Specific test should succeed."))
  t)

(defun test-persistent-list-add (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (loop for i from 1 to count
          for new-list = (add list i) then (add new-list i)
          do (assert (= (size new-list) i) () "Size of list should be ~D not ~D" i (size new-list))
             (assert (= (nth new-list -1) i) () "Last element of list should be ~D not ~D" i (nth new-list -1))))
  t)

(defun test-persistent-list-insert (list-constructor &key (fill-elt nil))
  (let ((list (insert (funcall list-constructor :fill-elt fill-elt) 5 :foo)))
    (assert (= (size list) 6) () "Insert should extend list.")
    (assert (eq (nth list 0) fill-elt) () "Empty elements should be filled with ~A." fill-elt)
    (let ((list (insert list 0 :bar)))
      (assert (= (size list) 7) () "Insert should increase length.")
      (assert (eq (nth list 0) :bar) () "Inserted element should be :BAR.")))
  t)

(defun test-persistent-list-insert-fill-zero (list-constructor)
  (test-persistent-list-insert list-constructor :fill-elt 0))

(defun test-persistent-list-insert-negative-index (list-constructor)
  (let ((list (add (funcall list-constructor) 0)))
    (let ((list (loop for i from 1 to 10
                      for new-list = (insert list (- i) i) then (insert new-list (- i) i)
                      finally (return new-list))))
      (let ((elts (loop for i below (size list) collect (nth list i)))
            (expected (loop for i from 10 downto 0 collect i)))
        (assert (equal elts expected) () "Inserted elements should be: ~A but found: ~A" expected elts))))
  t)

(defun test-persistent-list-insert-end (list-constructor)
  (let ((list (add (funcall list-constructor) 0 1 2)))
    (let ((list (insert list 3 3)))
      (assert (= (nth list 3) 3) () "Element at index ~D should be ~D" 3 3)
      (assert (= (size list) 4) () "Size of list should be ~D" 4)
      (let ((list (insert list 5 5)))
        (assert (= (nth list 5) 5) () "Element at index ~D should be ~D" 5 5)
        (assert (= (size list) 6) () "Size of list should be ~D" 6))))
  t)

(defun test-persistent-list-delete (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (let ((list (loop for n from count downto 0
                      for new-list = list then (delete new-list 0)
                      do (assert (= (size new-list) n) () "List size should reflect deletions")
                      finally (return new-list))))
      (print list)
      (assert (emptyp list) () "Empty list should be empty.")))
  t)

(defun test-persistent-list-delete-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (let ((list (loop for n from count downto 1
                      for (new-list deleted) = (multiple-value-list (delete list -1))
                        then (multiple-value-list (delete new-list -1))
                      do (assert (= deleted n) () "Deleted element should be last in list")
                      finally (return new-list))))
      (assert (emptyp list) () "Empty list should be empty.")))
  t)

(defun test-persistent-list-nth (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (loop for i from 0 below count
          do (assert (= (nth list i) (1+ i)) () "~:R element should be: ~A" i (1+ i))))
  t)

(defun test-persistent-list-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (loop for i from -1 downto (- count)
          do (assert (= (nth list i) (+ count i 1)) () "~:R element should be: ~D not ~D" i (+ count i 1) (nth list i))))
  t)

(defun test-persistent-list-setf-nth (list-constructor &optional (count 1000))
  (let ((list (loop for i from 0 to count
                    for new-list = (setf (nth (funcall list-constructor) i) i)
                      then (setf (nth new-list i) i)
                    finally (return new-list))))
    (loop for i from 0 to count
          do (assert (= (nth list i) i) () "Element ~D should have value ~D not ~D" i i (nth list i))))
  t)

(defun test-persistent-list-setf-nth-negative-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (let ((list (loop for i from -1 downto (- count)
                      for new-list = (setf (nth list i) i) then (setf (nth new-list i) i)
                      finally (return new-list))))
      (loop for i from 0 below count
            do (assert (= (nth list i) (- i count)) () "Element ~D should have value ~D not ~D" i (- i count) (nth list i)))) )
  t)

(defun test-persistent-list-setf-nth-out-of-bounds (list-constructor)
  (let ((list (setf (nth (funcall list-constructor) 10) :foo)))
    (assert (= (size list) (1+ 10)) () "List should expand to accommodate out-of-bounds index."))
  t)

(defun test-persistent-list-index (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (loop for i from 1 to count
          do (assert (= (index list i) (1- i)) () "The value ~D should be located at index ~D" (1- i) i)))
  t)

(defun test-persistent-list-index-test (list-constructor)
  (let ((list (apply #'add (funcall list-constructor) #[#\a #\z])))
    (assert (notany #'(lambda (ch) (index list ch)) #[#\A #\Z])
            ()
            "Default test EQL should fail.")
    (assert (every #'(lambda (ch) (index list ch :test #'char-equal)) #[#\A #\Z])
            ()
            "Specific test should succeed."))
  t)

(defun test-persistent-list-slice (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (let* ((n (floor count 2))
           (j (floor count 10))
           (slice (slice list j n)))
      (assert (= (size slice) n) () "Slice should contain ~D elements" n)
      (loop repeat n
            for i from 0
            do (assert (= (nth slice i) (+ i j 1)) () "Element ~D should have value ~D not ~D" i (+ i j 1) (nth slice i)))) )
  t)

(defun test-persistent-list-slice-corner-cases (list-constructor &optional (count 1000))
  (let ((list (fill (funcall list-constructor) count)))
    (let ((slice (slice list (size list) 10)))
      (assert (emptyp slice) () "Slice at end of list should be empty"))
    (let ((slice (slice list -10 10)))
      (assert (= (size slice) 10) () "Slice of last ~D elements should have ~D elements: ~D" 10 10 (size slice)))
    (let ((slice (slice list -1001 10)))
      (assert (emptyp slice) () "Slice with invalid negative index should be empty")))
  t)

(defun test-persistent-list-time (list-constructor)
  (let ((list (funcall list-constructor)))
    (time (dotimes (i 10 t)
            (loop for new-list = (fill list 10000) then (delete new-list 0)
                  until (emptyp new-list))))
    (time (dotimes (i 10 t)
            (loop for new-list = (fill list 10000) then (delete new-list -1)
                  until (emptyp new-list)))) ))

(deftest test-persistent-list ()
  (check
   (test-persistent-list-constructor #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-emptyp #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-size #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-clear #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-each #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-equals #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-equals-test #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-contains #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-contains-test #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-add #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-insert #'(lambda (&key fill-elt) (make-instance 'persistent-list :fill-elt fill-elt)))
   (test-persistent-list-insert-fill-zero #'(lambda (&key fill-elt) (make-instance 'persistent-list :fill-elt fill-elt)))
   (test-persistent-list-insert-negative-index #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-insert-end #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-delete #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-delete-negative-index #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-nth #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-nth-negative-index #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-setf-nth #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-setf-nth-out-of-bounds #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-setf-nth-negative-index #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-index #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-index-test #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-slice #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-slice-corner-cases #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-list-time #'(lambda () (make-instance 'persistent-list)))) )
