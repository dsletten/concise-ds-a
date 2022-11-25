;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               list-queue.lisp
;;;;
;;;;   Started:            Fri Nov  4 02:41:46 2022
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

(in-package :containers)

;;;
;;;    LINKED-LIST-QUEUE
;;;    - This is a TCONC queue wrapped by a list.
;;;    
(defclass linked-list-queue (queue)
  ((list :initform (make-linked-list-x)))) ; FILL-ELT is never used!!

(defmethod size ((q linked-list-queue))
  (with-slots (list) q
    (size list)))

;; (defmethod emptyp ((q linked-list-queue))
;;   (zerop (size q))) ; (null front)

(defmethod clear ((q linked-list-queue))
  (with-slots (list) q
    (clear list)))

(defmethod enqueue ((q linked-list-queue) obj)
  (with-slots (list) q
    (add list obj)))

(defmethod dequeue ((q linked-list-queue))
  (with-slots (list) q
    (delete list 0)))

(defmethod front ((q linked-list-queue))
  (with-slots (list) q
    (nth list 0)))

;;;
;;;    PERSISTENT-LIST-QUEUE
;;;
(let ((empty (make-persistent-list)))
  (defclass persistent-list-queue (queue)
    ((list :initform empty))))

(defmethod make-empty-persistent-queue ((q persistent-list-queue))
  (make-instance 'persistent-list-queue :type (type q)))

(defmethod size ((q persistent-list-queue))
  (with-slots (list) q
    (size list)))

;; (defmethod emptyp ((q persistent-list-queue))
;;   (zerop (size q)))

(defmethod clear ((q persistent-list-queue))
  (make-instance 'persistent-list-queue :type (type q)))

(defun initialize-list-queue (q list)
  (let ((new-queue (make-empty-persistent-queue q)))
    (with-slots ((new-list list)) new-queue
      (setf new-list list))
    new-queue))

(defmethod enqueue ((q persistent-list-queue) obj)
  (with-slots (list) q
    (initialize-list-queue q (add list obj)))) ; Not cheap. Particularly since there is no bulk ENQUEUE
                                        ; as with ADD!
(defmethod dequeue ((q persistent-list-queue))
  (with-slots (list) q
    (values (initialize-list-queue q (delete list 0)) (front q))))

(defmethod front ((q persistent-list-queue))
  (with-slots (list) q
    (nth list 0)))
