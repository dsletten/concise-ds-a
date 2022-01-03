;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               test-persistent-list-iterator.lisp
;;;;
;;;;   Started:            Mon Dec 27 17:47:03 2021
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

(defun test-persistent-list-iterator-vs-iterator (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (iterator list))
         (list-iterator (list-iterator list)))
    (let ((list-iterator (loop for new-iterator = iterator then (next new-iterator)
                               for new-list-iterator = list-iterator then (next new-list-iterator)
                               until (done new-iterator)
                               do (assert (eq (current iterator) (current list-iterator))
                                          ()
                                          "Iterator elements should be identical: ~A vs ~A"
                                          (current iterator)
                                          (current list-iterator))
                              finally (return new-list-iterator))))
      (assert (null list-iterator)) () "Iterators should both be exhausted"))
;      (assert (not (has-next list-iterator)) () "Iterators should both be exhausted")))
  t)

;; (defun test-persistent-emptyp (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (assert (emptyp list) () "New list should be empty.")
;;     (add list t)
;;     (assert (not (emptyp list)) () "List with elt should not be empty.")
;;     (delete list 0)
;;     (assert (emptyp list) () "Empty list should be empty.")
;;     t))

(defun test-persistent-forward-traversal (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((iterator (list-iterator list)))
      (loop for i from 1 upto count
            do (assert (= (current iterator) i) () "Current element of iterator should be ~D not ~D" i (current iterator))
               (next iterator))
      (assert (not (has-next iterator)) () "Iterator should be at end of list")))
  t)

(defun test-persistent-backward-traversal (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((iterator (list-iterator list (1- count))))
      (loop for i from count downto 1
            do (assert (= (current iterator) i) () "Current element of iterator should be ~D not ~D" i (current iterator))
               (previous iterator))
      (assert (not (has-previous iterator)) () "Iterator should be at beginning of list")))
  t)

(defun test-persistent-remove-forward (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((iterator (list-iterator list)))
      (loop for i from 1 upto count
            until (emptyp iterator)
            do (assert (zerop (current-index iterator)) () "CURRENT-INDEX should be ~D not ~D" 0 (current-index iterator))
               (let ((doomed (remove iterator)))
                 (assert (= doomed i) () "Removed element should be ~D not ~D" i doomed))
               (assert (= (size list) (- count i)) () "The list should decrease in size."))))
  t)

(defun test-persistent-remove-backward (list-constructor &optional (count 1000))
  (let ((list (funcall list-constructor)))
    (fill-list list count)
    (let ((iterator (list-iterator list (1- count))))
      (loop for i from (1- count) downto 0
            until (emptyp iterator)
            do (assert (= (current-index iterator) i) () "CURRENT-INDEX should be ~D not ~D" i (current-index iterator))
               (let ((doomed (remove iterator)))
                 (assert (= doomed (1+ i)) () "Removed element should be ~D not ~D" (1+ i) doomed))
               (assert (= (size list) i) () "The list should decrease in size."))))
  t)

(defun test-persistent-remove-inside-out (list-constructor &optional (count 1001))
  (let* ((list (funcall list-constructor))
         (mid (truncate count 2))
         (expected (append (loop for i from (1+ mid) upto count collect i)
                           (loop for i from mid downto 1 collect i))))
    (fill-list list count)
    (let ((iterator (list-iterator list mid)))
      (loop for i from count downto 1
            for elt in expected
            until (emptyp iterator)
            do (assert (= (size list) i) () "The list should decrease in size.")
               (assert (= (current iterator) elt) () "CURRENT should be ~D not ~D" elt (current iterator))
               (if (has-next iterator)
                   (assert (= (current-index iterator) mid) () "CURRENT-INDEX should be ~D not ~D" mid (current-index iterator))
                   (assert (= (current-index iterator) (1- i)) () "CURRENT-INDEX should be ~D not ~D" (1- i) (current-index iterator)))
               (remove iterator))))
  t)

(defun test-persistent-add-before-empty (list-constructor &optional (count 1000))
  (let* ((list (funcall list-constructor))
         (iterator (list-iterator list)))
      (loop for i from 1 upto count
            do (add-before iterator i)
               (assert (= (size list) i) () "The list should increase in size.")
               (assert (= (current-index iterator) (1- i)) () "CURRENT-INDEX should be ~D not ~D" (1- i) (current-index iterator)))
      (let ((expected (cons 1 (loop for i from count above 1 collect i))))
        (loop for elt in expected
              do (assert (= (current iterator) elt) () "Current element should be ~D not ~D" elt (current iterator))
                 (previous iterator))))
  t)

;;
;;    (eq (current *li*) (nth *al* (current-index *li*)))


(defun test-persistent-add-after-empty (list-constructor &optional (count 1000))
  (let* ((list (funcall list-constructor))
         (iterator (list-iterator list)))
    (loop for i from 1 upto count
          do (add-after iterator i)
             (assert (= (size list) i) () "The list should decrease in size.")
             (assert (= (current-index iterator) 0) () "CURRENT-INDEX should be ~D not ~D" 0 (current-index iterator)))
    (let ((expected (cons 1 (loop for i from count above 1 collect i))))
      (loop for elt in expected
            do (assert (= (current iterator) elt) () "Current element should be ~D not ~D" elt (current iterator))
               (next iterator))))
  t)

;; (defun test-persistent-add-before-inside-out (list-constructor &optional (count 1001))
;;   (let* ((list (funcall list-constructor))
;;          (mid (truncate count 2))
;;          (expected (append (loop for i from (1+ mid) upto count collect i)
;;                            (loop for i from mid downto 1 collect i))))
;;     (fill-list list count)
;;     (let ((iterator (list-iterator list mid)))
;;       (loop for i from count downto 1
;;             for elt in expected
;;             until (emptyp iterator)
;;             do (assert (= (size list) i) () "The list should decrease in size.")
;;                (assert (= (current iterator) elt) () "CURRENT should be ~D not ~D" elt (current iterator))
;;                (if (has-next iterator)
;;                    (assert (= (current-index iterator) mid) () "CURRENT-INDEX should be ~D not ~D" mid (current-index iterator))
;;                    (assert (= (current-index iterator) (1- i)) () "CURRENT-INDEX should be ~D not ~D" (1- i) (current-index iterator)))
;;                (add-before iterator))))
;;   t)

(deftest test-persistent-list-list-iterator ()
  (check
   (test-persistent-list-iterator-vs-iterator #'(lambda () (make-instance 'persistent-list)))
   ;; (test-persistent-forward-traversal #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-backward-traversal #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-remove-forward #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-remove-backward #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-remove-inside-out #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-add-before-empty #'(lambda () (make-instance 'array-list)))
   ;; (test-persistent-add-after-empty #'(lambda () (make-instance 'array-list)))

))
