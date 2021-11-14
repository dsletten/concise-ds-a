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
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :containers)

(use-package :test)

(defun test-persistent-constructor (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (handler-case (delete list 0)
      (error (e)
        (format t "Got expected error: ~A~%" e))
      (:no-error (obj)
        (declare (ignore obj))
        (error "Can't call DELETE on empty list.~%")))
    t))

(defun test-persistent-emptyp (list-constructor)
  (let ((list (funcall list-constructor)))
    (assert (emptyp list) () "New list should be empty.")
    (assert (not (emptyp (add list t))) () "List with elt should not be empty.")
    (assert (emptyp (delete (add list t) 0)) () "Empty list should be empty.")
    t))

(defun test-persistent-size (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (assert (zerop (size list)) () "Size of new list should be zero.")
    (loop for i from 1 to count
          for new-list = (push list i) then (push new-list i)   ; ??????? Scope problem without renaming?!??!
          do (assert (= (size new-list) i) () "Size of list should be ~D." i)
          finally (return t))))

(defun test-persistent-clear (list-constructor &optional (count 1000))
  (let ((list (fill-persistent-list (funcall list-constructor) count)))
    (assert (not (emptyp list)) () "List should have ~D elements." count)
    (assert (emptyp (clear list)) () "List should be empty."))
  t)

(defun fill-persistent-list (list count)
  (loop for i from 1 to count
        for new-list = (push list i) then (push new-list i)   ; ??????? Scope problem without renaming?!??!
        finally (return new-list)))

(defun test-persistent-pop (list-constructor &optional (count 1000))
  (let ((list (fill-persistent-list (funcall list-constructor) count)))
    (loop for i from (size list) downto 1
          for (new-list popped) = (multiple-value-list (pop list)) then (multiple-value-list (pop new-list)) 
          unless (= i popped)
          do (error "Wrong value on list: ~A should be: ~A~%" popped i)
          finally (assert (emptyp new-list) () "List should be empty.")))
  t)

(defun test-persistent-top (list-constructor &optional (count 1000))
  (let ((list (fill-persistent-list (funcall list-constructor) count)))
    (loop for i from (size list) downto 1
          for top = (top list)
          unless (= i top)
          do (error "Wrong value on list: ~A should be: ~A~%" top i)
          do (setf list (pop list)))
    (assert (emptyp list) () "List should be empty."))
  t)

(deftest test-persistent-list ()
  (check
   (test-persistent-constructor #'(lambda () (make-instance 'persistent-list)))
   (test-persistent-emptyp #'(lambda () (make-instance 'persistent-list)))
   ;; (test-persistent-size #'(lambda () (make-instance 'persistent-list)))
   ;; (test-persistent-clear #'(lambda () (make-instance 'persistent-list)))
   ;; (test-persistent-pop #'(lambda () (make-instance 'persistent-list)))
   ;; (test-persistent-top #'(lambda () (make-instance 'persistent-list)))) )
))
