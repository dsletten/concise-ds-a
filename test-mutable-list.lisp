;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               test-mutable-list.lisp
;;;;
;;;;   Started:            Wed Dec 21 02:06:04 2022
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
;;;;   Notes: This is somewhat redundant. All lists that are not persistent are mutable, so these
;;;;   could be integrated with test-list.lisp?
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

(defun test-mutable-list-clear (list-constructor)
  (let ((list (funcall list-constructor)))
    (with-slots (modification-count) list
      (assert (zerop modification-count) () "New list should not have been modified")
      (fill list :count 20)
      (clear list)
      (assert (= 2 modification-count) () "Clearing filled list modifies it twice")
      (clear list)
      (assert (= 2 modification-count) () "Clearing empty list does not modify it again")))
  t)

(defun test-mutable-list-add (list-constructor)
  (let ((list (funcall list-constructor)))
    (with-slots (modification-count) list
      (add list 2 4 6 8)
      (assert (= 1 modification-count) () "Adding multiple items is one modification")
      (add list 10)
      (assert (= 2 modification-count) () "Subsequent addition is another modification")))
  t)

(defun test-mutable-list-insert (list-constructor)
  (let ((list (funcall list-constructor)))
    (with-slots (modification-count) list
      (insert list 0 99)
      (assert (= 1 modification-count) () "Inserting into empty list causes modification")
      (insert list -5 88)
      (assert (= 1 modification-count) () "Invalid insertion does not cause modification")))
  t)

(defun test-mutable-list-delete (list-constructor)
  (let ((list (fill (funcall list-constructor) :count 20)))
    (with-slots (modification-count) list
      (delete list 0)
      (assert (= 2 modification-count) () "Deleting from filled list modifies it twice")
      (delete list -1)
      (assert (= 3 modification-count) () "Subsequent deletion is another modification")
      (delete list -20)
      (assert (= 3 modification-count) () "Invalid deletion does not cause modification")))
  t)

(deftest mutable-list-test-suite (constructor)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-mutable-list-clear
                 test-mutable-list-add
                 test-mutable-list-insert
                 test-mutable-list-delete)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor)))) )))

(deftest test-mutable-array-list ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'array-list)))) )

(deftest test-mutable-array-list-x ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'array-list-x)))) )

(deftest test-mutable-singly-linked-list ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'singly-linked-list)))) )

(deftest test-mutable-singly-linked-list-x ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'singly-linked-list-x)))) )

(deftest test-mutable-doubly-linked-list ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'doubly-linked-list)))) )

(deftest test-mutable-doubly-linked-list-ratchet ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'doubly-linked-list-ratchet)))) )

(deftest test-mutable-doubly-linked-list-hash-table ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'doubly-linked-list-hash-table)))) )

(deftest test-mutable-hash-table-list ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'hash-table-list)))) )

(deftest test-mutable-hash-table-list-x ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'hash-table-list-x)))) )

(deftest test-mutable-hash-table-list-z ()
  (check
   (mutable-list-test-suite #'(lambda () (make-instance 'hash-table-list-z)))) )

(deftest test-mutable-list-all ()
  (check
   (test-mutable-array-list)
   (test-mutable-array-list-x)
   (test-mutable-singly-linked-list)
   (test-mutable-singly-linked-list-x)
   (test-mutable-doubly-linked-list)
   (test-mutable-doubly-linked-list-ratchet)
   (test-mutable-doubly-linked-list-hash-table)
   (test-mutable-hash-table-list)
   (test-mutable-hash-table-list-x)
   (test-mutable-hash-table-list-z)))

      
      
  

