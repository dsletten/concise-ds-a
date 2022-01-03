;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               test-persistent-iterator.lisp
;;;;
;;;;   Started:            Sun Dec 26 22:49:53 2021
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

(defun test-persistent-iterator (collection)
  (let ((iterator (iterator collection))
        (size (size collection)))
    (let ((iterator (loop repeat size
                          for new-iterator = iterator then (next new-iterator)
                          do (assert (not (done new-iterator)) () "Iterator should not yet be exhausted.")
                          finally (return (next new-iterator)))) )
      (assert (done iterator) () "Iterator should be exhausted.")))
  t)

(defun test-persistent-empty-iterator (collection-constructor)
  (let ((iterator (iterator (funcall collection-constructor))))
    (assert (done iterator) () "Iterator for empty collection should be exhausted."))
  t)

(defun test-persistent-iterator-contents (collection contents)
  (let ((iterator (iterator collection)))
    (loop for current = (current iterator) then (current new-iterator)
          for new-iterator = (next iterator) then (next new-iterator)
          for elt in contents
          for tail on (rest contents)
          do (assert (= elt current) () "Current iterator element should be ~D not ~D" elt current)
          finally (progn (assert (null tail) () "All elements should have been consumed.")
                         (assert (done new-iterator) () "Iterator should be exhausted."))))
  t)

(deftest test-persistent-list-iterator ()
  (check
   (let ((list (fill (make-instance 'persistent-list) 1000)))
     (test-persistent-iterator list))
   (test-persistent-empty-iterator #'(lambda () (make-instance 'persistent-list)))
   (let ((list (make-instance 'persistent-list))
         (contents (make-random-contents)))
     (test-persistent-iterator-contents (apply #'add list contents) contents))))
