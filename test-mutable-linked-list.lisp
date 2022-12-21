;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               test-mutable-linked-list.lisp
;;;;
;;;;   Started:            Tue Dec 20 02:46:32 2022
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

(defun test-mutable-linked-list-insert-before (list-constructor get-node next-node)
  (let ((list (funcall list-constructor :type 'integer :fill-elt 0)))
    (handler-case (insert-before list nil :foo)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call INSERT-BEFORE with wrong type.~%")))
    (handler-case (insert-before list nil 8)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call INSERT-BEFORE on empty list.~%")))
    (fill list :count 20)
    (handler-case (insert-before list nil 8)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't INSERT-BEFORE NULL node.~%")))
    (let* ((start (funcall get-node list))
           (child (funcall next-node list start))
           (grand-child (funcall next-node list child)))
      (insert-before list grand-child -99)
      (assert (= -99 (nth list 2)) () "Element after child should be -99")))
;      (assert (not (eq (funcall next-node child) grand-child)) () "Element should replace grandchild.")))
  t)

(defun test-mutable-linked-list-insert-after (list-constructor get-node next-node)
  (let ((list (funcall list-constructor :type 'integer :fill-elt 0)))
    (handler-case (insert-after list nil :foo)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call INSERT-AFTER with wrong type.~%")))
    (handler-case (insert-after list nil 8)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call INSERT-AFTER on empty list.~%")))
    (fill list :count 20)
    (handler-case (insert-after list nil 8)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't INSERT-AFTER NULL node.~%")))
    (let* ((start (funcall get-node list))
           (child (funcall next-node list start)))
      (insert-after list child -99)
      (assert (= -99 (nth list 2)) () "Element after child should be -99")))
  t)

(defun test-mutable-linked-list-delete-node (list-constructor get-node next-node)
  (let ((list (funcall list-constructor :type 'integer :fill-elt 0)))
    (handler-case (delete-node list nil)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DELETE-NODE on empty list.~%")))
    (fill list :count 20)
    (handler-case (delete-node list nil)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't DELETE-NODE for NULL node.~%")))
    (let* ((start (funcall get-node list))
           (child (funcall next-node list start))
           (grand-child (funcall next-node list child)))
      (delete-node list grand-child)
      (assert (= 4 (nth list 2)) () "Element after child should be 3")))
  t)

(defun test-mutable-linked-list-delete-child (list-constructor get-node next-node)
  (let ((list (funcall list-constructor :type 'integer :fill-elt 0)))
    (handler-case (delete-child list nil)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DELETE-CHILD on empty list.~%")))
    (fill list :count 20)
    (handler-case (delete-child list nil)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't DELETE-CHILD of NULL node.~%")))
    (let* ((start (funcall get-node list))
           (child (funcall next-node list start)))
      (delete-child list child)
      (assert (= 4 (nth list 2)) () "Element after child should be 3")))
  t)

(deftest mutable-linked-list-test-suite (constructor get-node next-node)
  (format t "Testing ~A~%" (class-name (class-of (funcall constructor))))
  (let ((tests '(test-mutable-linked-list-insert-before
                 test-mutable-linked-list-insert-after
                 test-mutable-linked-list-delete-node
                 test-mutable-linked-list-delete-child)))
    (notany #'null (loop for test in tests
                         collect (progn
                                  (format t "~A~%" test)
                                  (check (funcall test constructor get-node next-node)))) )))

(deftest test-mll-singly-linked-list ()
  (check
   (mutable-linked-list-test-suite #'(lambda (&key (type t) fill-elt) (make-instance 'singly-linked-list :type type :fill-elt fill-elt))
                                   #'(lambda (list) (slot-value list 'store))
                                   #'(lambda (list node) (declare (ignore list)) (rest node)))) )

(deftest test-mll-singly-linked-list-x ()
  (check
   (mutable-linked-list-test-suite #'(lambda (&key (type t) fill-elt) (make-instance 'singly-linked-list-x :type type :fill-elt fill-elt))
                                   #'(lambda (list) (slot-value list 'front))
                                   #'(lambda (list node) (declare (ignore list)) (rest node)))) )

(deftest test-mll-doubly-linked-list ()
  (check
   (mutable-linked-list-test-suite #'(lambda (&key (type t) fill-elt) (make-instance 'doubly-linked-list :type type :fill-elt fill-elt))
                                   #'(lambda (list) (slot-value list 'store))
                                   #'(lambda (list node) (declare (ignore list)) (next node)))) )

(deftest test-mll-doubly-linked-list-ratchet ()
  (check
   (mutable-linked-list-test-suite #'(lambda (&key (type t) fill-elt) (make-instance 'doubly-linked-list-ratchet :type type :fill-elt fill-elt))
                                   #'(lambda (list) (slot-value list 'store))
                                   #'(lambda (list node) (declare (ignore list)) (next node)))) )

(deftest test-mll-doubly-linked-list-hash-table ()
  (check
   (mutable-linked-list-test-suite #'(lambda (&key (type t) fill-elt) (make-instance 'doubly-linked-list-hash-table :type type :fill-elt fill-elt))
                                   #'(lambda (list) (slot-value list 'head))
                                   #'(lambda (list node) (next-dnode list node)))) )

(deftest test-mutable-linked-list-all ()
  (check
   (test-mll-singly-linked-list)
   (test-mll-singly-linked-list-x)
   (test-mll-doubly-linked-list)
   (test-mll-doubly-linked-list-ratchet)
   (test-mll-doubly-linked-list-hash-table)))
