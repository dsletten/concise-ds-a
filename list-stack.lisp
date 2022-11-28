;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               list-stack.lisp
;;;;
;;;;   Started:            Fri Nov  4 02:41:43 2022
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
;;;    LINKED-LIST-STACK
;;;
;;;    Complications with layering data structures...
;;;    (setf *slls* (make-instance 'linked-list-stack :type 'integer))
;;;      Incompatible FILL-ELT type: NULL should be: INTEGER
;;;    (setf *slls* (make-instance 'linked-list-stack :type 'integer :fill-elt 0))
;;;      Invalid initialization argument:
;;;          :FILL-ELT
(defclass linked-list-stack (stack)
  ((list :initform (make-linked-list)))) ; FILL-ELT is never used!!

(defmethod size ((s linked-list-stack))
  (with-slots (list) s
    (size list)))

(defmethod emptyp ((s linked-list-stack))
  (with-slots (list) s
    (emptyp list)))

(defmethod clear ((s linked-list-stack))
  (with-slots (list) s
    (clear list)))

(defmethod push ((s linked-list-stack) obj)
  (with-slots (list) s
    (insert list 0 obj)))

(defmethod pop ((s linked-list-stack))
  (with-slots (list) s
    (delete list 0)))

(defmethod peek ((s linked-list-stack))
  (with-slots (list) s
    (nth list 0)))

;;;
;;;    PERSISTENT-LIST-STACK
;;;
(let ((empty (make-persistent-list))) ; FILL-ELT is never used!!
  (defclass persistent-list-stack (persistent-stack)
    ((list :initform empty))))

(defmethod make-empty-persistent-stack ((q persistent-list-stack))
  (make-instance 'persistent-list-stack :type (type q)))

(defmethod size ((s persistent-list-stack))
  (with-slots (list) s
    (size list)))

(defmethod emptyp ((s persistent-list-stack))
  (with-slots (list) s
    (emptyp list)))

(defmethod clear ((s persistent-list-stack))
  (make-empty-persistent-stack s))

(flet ((initialize-stack (s list)
         (let ((new-stack (make-empty-persistent-stack s)))
           (with-slots ((new-list list)) new-stack
             (setf new-list list))
           new-stack)))
  (defmethod push ((s persistent-list-stack) obj)
    (with-slots (list) s
      (initialize-stack s (insert list 0 obj))))
  (defmethod pop ((s persistent-list-stack))
    (with-slots (list) s
      (values (initialize-stack s (delete list 0)) (peek s)))) )

(defmethod peek ((s persistent-list-stack))
  (with-slots (list) s
    (nth list 0)))


