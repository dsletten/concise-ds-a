;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               storage.lisp
;;;;
;;;;   Started:            Sat Dec 19 21:25:09 2020
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

(defpackage :storage (:use :common-lisp :test))

(in-package :storage)

(defparameter *lockers* 138)

;;;
;;;    @inv: (= (length rented) *lockers*)
;;;    @inv: (<= 0 available-lockers *lockers*)
;;;    
(defclass storage-facility ()
  ((rented :initform (make-array *lockers* :element-type 'boolean :initial-element nil))
   (available-lockers :reader available-lockers :initform *lockers*)))

(defgeneric capacity (storage-facility)
  (:documentation "Determine the capacity of the storage facility."))
(defmethod capacity ((storage storage-facility))
  (with-slots (rented) storage
    (length rented)))

;;;
;;;    Find an empty locker, mark it rented, return its number
;;;    @pre: (not (fullp storage))
;;;    @post: (= available-lockers (1- old-available-lockers))
;;;    
(defgeneric rent-locker (storage-facility)
  (:documentation "Rent out the first available locker."))
(defmethod rent-locker :around ((storage storage-facility))
  (if (fullp storage)
      (error "Storage facility is full.")
      (call-next-method)))
(defmethod rent-locker ((storage storage-facility))
  (with-slots (rented available-lockers) storage
    (let ((locker-number (position nil rented)))
      (setf (aref rented locker-number) t)
      (decf available-lockers)
      locker-number)))

(defgeneric valid-locker-p (storage-facility locker)
  (:documentation "Is the specified LOCKER valid?"))
(defmethod valid-locker-p ((storage storage-facility) (locker-number integer))
  (<= 0 locker-number (1- (capacity storage))))

;;;
;;;    Mark a locker as no longer rented
;;;    @pre: (and (valid-locker-p storage locker-number)
;;;               (not (freep locker-number)))
;;;    @post: (and (freep locker-number)
;;;           (= available-lockers (1+ old-available-lockers))
;;;    
(defgeneric release-locker (storage locker-number)
  (:documentation "Free the specified locker."))
(defmethod release-locker :around ((storage storage-facility) (locker-number integer)) ; Already free????
  (cond ((not (valid-locker-p storage locker-number)) (error "Invalid locker: ~D" locker-number))
        ((freep storage locker) (error "Locker is not in use: ~D" locker-number))
        (t (call-next-method))))
(defmethod release-locker ((storage storage-facility) (locker-number integer))
  (with-slots (rented available-lockers) storage
    (setf (aref rented locker-number) nil)
    (incf available-lockers)))
    
;;;
;;;    Say whether a locker is for rent
;;;    @pre: (valid-locker-p storage locker-number)
;;;    
(defgeneric freep (storage-facility locker)
  (:documentation "Is the specified LOCKER free?"))
(defmethod freep :around ((storage storage-facility) (locker-number integer))
  (if (valid-locker-p storage locker-number)
      (call-next-method)
      (error "Invalid locker: ~D" locker-number)))
(defmethod freep ((storage storage-facility) (locker-number integer))
  (with-slots (rented) storage
    (not (aref rented locker-number))))

;;;
;;;    Say whether any lockers are left to rent
;;;    
(defgeneric fullp (storage-facility)
  (:documentation "Is the storage facility full?"))
(defmethod fullp ((storage storage-facility))
  (zerop (available-lockers storage)))
