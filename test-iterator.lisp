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

(defun test-iterator-contents (collection contents)
  (let ((iterator (iterator collection)))
    (loop for elt in contents
          for tail on (rest contents)
          until (done iterator)
          do (assert (= elt (current iterator)) () "Current iterator element should be ~D not ~D" elt (current iterator))
             (next iterator)
          finally (assert (null tail) () "All elements should have been consumed.")))
  t)

(defun make-random-contents (&optional (count 1000))
  (loop repeat count
        with random-state = (make-random-state t)
        collect (random 1d0 random-state)))

(deftest test-array-list-iterator ()
  (check
   (let ((al (make-instance 'array-list)))
     (fill al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'array-list)))
   (let ((al (make-instance 'array-list))
         (contents (make-random-contents)))
     (apply #'add al contents)
     (test-iterator-contents al contents))))

(deftest test-array-list-x-iterator ()
  (check
   (let ((al (make-instance 'array-list-x)))
     (fill al 1000)
     (test-iterator al))
   (test-empty-iterator #'(lambda () (make-instance 'array-list-x)))
   (let ((al (make-instance 'array-list-x))
         (contents (make-random-contents)))
     (apply #'add al contents)
     (test-iterator-contents al contents))))

(deftest test-singly-linked-list-iterator ()
  (check
   (let ((sll (make-instance 'singly-linked-list)))
     (fill sll 1000)
     (test-iterator sll))
   (test-empty-iterator #'(lambda () (make-instance 'singly-linked-list)))
   (let ((sll (make-instance 'singly-linked-list))
         (contents (make-random-contents)))
     (apply #'add sll contents)
     (test-iterator-contents sll contents))))

(deftest test-singly-linked-list-x-iterator ()
  (check
   (let ((sllx (make-instance 'singly-linked-list-x)))
     (fill sllx 1000)
     (test-iterator sllx))
   (test-empty-iterator #'(lambda () (make-instance 'singly-linked-list-x)))
   (let ((sllx (make-instance 'singly-linked-list-x))
         (contents (make-random-contents)))
     (apply #'add sllx contents)
     (test-iterator-contents sllx contents))))

(deftest test-doubly-linked-list-iterator ()
  (check
   (let ((dll (make-instance 'doubly-linked-list)))
     (fill dll 1000)
     (test-iterator dll))
   (test-empty-iterator #'(lambda () (make-instance 'doubly-linked-list)))
   (let ((dll (make-instance 'doubly-linked-list))
         (contents (make-random-contents)))
     (apply #'add dll contents)
     (test-iterator-contents dll contents))))

(deftest test-hash-table-list-iterator ()
  (check
   (let ((htl (make-instance 'hash-table-list)))
     (fill htl 1000)
     (test-iterator htl))
   (test-empty-iterator #'(lambda () (make-instance 'hash-table-list)))
   (let ((htl (make-instance 'hash-table-list))
         (contents (make-random-contents)))
     (apply #'add htl contents)
     (test-iterator-contents htl contents))))

(deftest test-hash-table-list-x-iterator ()
  (check
   (let ((htlx (make-instance 'hash-table-list-x)))
     (fill htlx 1000)
     (test-iterator htlx))
   (test-empty-iterator #'(lambda () (make-instance 'hash-table-list-x)))
   (let ((htlx (make-instance 'hash-table-list-x))
         (contents (make-random-contents)))
     (apply #'add htlx contents)
     (test-iterator-contents htlx contents))))

(deftest test-hash-table-list-z-iterator ()
  (check
   (let ((htlz (make-instance 'hash-table-list-z)))
     (fill htlz 1000)
     (test-iterator htlz))
   (test-empty-iterator #'(lambda () (make-instance 'hash-table-list-z)))
   (let ((htlz (make-instance 'hash-table-list-z))
         (contents (make-random-contents)))
     (apply #'add htlz contents)
     (test-iterator-contents htlz contents))))

(deftest test-iterator-all ()
  (check
   (test-array-list-iterator)
   (test-array-list-x-iterator)
   (test-singly-linked-list-iterator)
   (test-singly-linked-list-x-iterator)
   (test-doubly-linked-list-iterator)
   (test-hash-table-list-iterator)
   (test-hash-table-list-x-iterator)
   (test-hash-table-list-z-iterator)))
