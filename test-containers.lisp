;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               test-containers.lisp
;;;;
;;;;   Started:            Wed Jan  6 00:53:48 2021
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

;; (defpackage :test-containers 
;;   (:shadowing-import-from :containers :type :push :pop)
;;   (:use :common-lisp :containers :test))

;; (in-package :test-containers)

(in-package :containers)

(use-package :test)

(defgeneric fill (container &optional count)
  (:documentation "Fill up a container for testing purposes."))
(defmethod fill ((c collection) &optional count)
  (declare (ignore c count))
  (error "COLLECTION does not implement FILL"))

(deftest test-array-stack-size ()
  (let ((stack (make-instance 'array-stack :type 'integer))
        (elements '(2 4 6 8 10)))
    (check (zerop (size stack)))
    (loop for element in elements
          for size from 1
          do (push stack element)
             (check (= (size stack) size)))) )

(deftest test-array-stack-emptyp ()
  (let ((stack (make-instance 'array-stack :type 'integer)))
    (check
     (emptyp stack)
     (progn (push stack 9) (not (emptyp stack)))
     (progn (pop stack) (emptyp stack)))) )

(deftest test-linked-stack-size ()
  (let ((stack (make-instance 'linked-stack :type 'integer))
        (elements '(2 4 6 8 10)))
    (check (zerop (size stack)))
    (loop for element in elements
          for size from 1
          do (push stack element)
             (check (= (size stack) size)))) )

(deftest test-linked-stack-emptyp ()
  (let ((stack (make-instance 'linked-stack :type 'integer)))
    (check
     (emptyp stack)
     (progn (push stack 9) (not (emptyp stack)))
     (progn (pop stack) (emptyp stack)))) )

(deftest test-hash-table-stack-size ()
  (let ((stack (make-instance 'hash-table-stack :type 'integer))
        (elements '(2 4 6 8 10)))
    (check (zerop (size stack)))
    (loop for element in elements
          for size from 1
          do (push stack element)
             (check (= (size stack) size)))) )

(deftest test-hash-table-stack-emptyp ()
  (let ((stack (make-instance 'hash-table-stack :type 'integer)))
    (check
     (emptyp stack)
     (progn (push stack 9) (not (emptyp stack)))
     (progn (pop stack) (emptyp stack)))) )

