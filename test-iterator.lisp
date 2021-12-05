;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               test-iterator.lisp
;;;;
;;;;   Started:            Sat Dec  4 19:08:17 2021
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

(defun test-iterator (collection)
  (let ((iterator (iterator collection))
        (size (size collection)))
    (loop repeat size
          do (assert (not (done iterator)) () "Iterator should not yet be exhausted.")
             (next iterator))
    (assert (done iterator) () "Iterator should be exhausted."))
  t)

(defun test-empty-iterator (collection-constructor)
  (let ((iterator (iterator (funcall collection-constructor))))
    (assert (done iterator) () "Iterator for empty collection should be exhausted."))
  t)

(deftest test-array-list-iterator ()
  (check
   (let ((al (make-instance 'array-list)))
     (fill-list al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'array-list)))) )

(deftest test-singly-linked-list-iterator ()
  (check
   (let ((al (make-instance 'singly-linked-list)))
     (fill-list al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'singly-linked-list)))) )

(deftest test-doubly-linked-list-iterator ()
  (check
   (let ((al (make-instance 'doubly-linked-list)))
     (fill-list al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'doubly-linked-list)))) )

(deftest test-hash-table-list-iterator ()
  (check
   (let ((al (make-instance 'hash-table-list)))
     (fill-list al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'hash-table-list)))) )


