;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               test-list-iterator.lisp
;;;;
;;;;   Started:            Thu Dec  2 11:52:54 2021
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

(defun test-list-iterator-vs-iterator (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (iterator list))
         (list-iterator (list-iterator list)))
    (loop until (done iterator)
          do (assert (eq (current iterator) (current list-iterator))
                     ()
                     "Iterator elements should be identical: ~A vs ~A"
                     (current iterator)
                     (current list-iterator))
             (next iterator)
             (next list-iterator))
    (assert (not (has-next list-iterator)) () "Iterators should both be exhausted"))
  t)

;; (defun test-emptyp (list-constructor)
;;   (let ((list (funcall list-constructor)))
;;     (assert (emptyp list) () "New list should be empty.")
;;     (add list t)
;;     (assert (not (emptyp list)) () "List with elt should not be empty.")
;;     (delete list 0)
;;     (assert (emptyp list) () "Empty list should be empty.")
;;     t))

(defun test-forward-traversal (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (list-iterator list)))
    (loop for i from 1 upto count
          do (assert (= (current iterator) i) () "Current element of iterator should be ~D not ~D" i (current iterator))
             (next iterator))
    (assert (not (has-next iterator)) () "Iterator should be at end of list"))
  t)

(defun test-backward-traversal (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (list-iterator list (1- count))))
    (loop for i from count downto 1
          do (assert (= (current iterator) i) () "Current element of iterator should be ~D not ~D" i (current iterator))
             (previous iterator))
    (assert (not (has-previous iterator)) () "Iterator should be at beginning of list"))
  t)

(defun test-remove-forward (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (list-iterator list)))
    (loop for i from 1 upto count
          until (emptyp iterator)
          do (assert (zerop (current-index iterator)) () "CURRENT-INDEX should be ~D not ~D" 0 (current-index iterator))
             (let ((doomed (remove iterator)))
               (assert (= doomed i) () "Removed element should be ~D not ~D" i doomed))
         (assert (= (size list) (- count i)) () "The list should decrease in size.")))
  t)

(defun test-remove-backward (list-constructor &optional (count 1000))
  (let* ((list (fill (funcall list-constructor) count))
         (iterator (list-iterator list (1- count))))
    (loop for i from (1- count) downto 0
          until (emptyp iterator)
          do (assert (= (current-index iterator) i) () "CURRENT-INDEX should be ~D not ~D" i (current-index iterator))
             (let ((doomed (remove iterator)))
               (assert (= doomed (1+ i)) () "Removed element should be ~D not ~D" (1+ i) doomed))
         (assert (= (size list) i) () "The list should decrease in size.")))
  t)

(defun test-remove-inside-out (list-constructor &optional (count 1001))
  (let* ((list (fill (funcall list-constructor) count))
         (mid (truncate count 2))
         (expected (append (loop for i from (1+ mid) upto count collect i)
                           (loop for i from mid downto 1 collect i)))
         (iterator (list-iterator list mid)))
    (loop for i from count downto 1
          for elt in expected
          until (emptyp iterator)
          do (assert (= (size list) i) () "The list should decrease in size.")
             (assert (= (current iterator) elt) () "CURRENT should be ~D not ~D" elt (current iterator))
             (if (has-next iterator)
                 (assert (= (current-index iterator) mid) () "CURRENT-INDEX should be ~D not ~D" mid (current-index iterator))
                 (assert (= (current-index iterator) (1- i)) () "CURRENT-INDEX should be ~D not ~D" (1- i) (current-index iterator)))
             (remove iterator)))
  t)

(defun test-add-before-empty (list-constructor &optional (count 1000))
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


(defun test-add-after-empty (list-constructor &optional (count 1000))
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

;; (defun test-add-before-inside-out (list-constructor &optional (count 1001))
;;   (let* ((list (funcall list-constructor))
;;          (mid (truncate count 2))
;;          (expected (append (loop for i from (1+ mid) upto count collect i)
;;                            (loop for i from mid downto 1 collect i))))
;;     (fill list count)
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

(deftest test-array-list-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'array-list)))
   (test-forward-traversal #'(lambda () (make-instance 'array-list)))
   (test-backward-traversal #'(lambda () (make-instance 'array-list)))
   (test-remove-forward #'(lambda () (make-instance 'array-list)))
   (test-remove-backward #'(lambda () (make-instance 'array-list)))
   (test-remove-inside-out #'(lambda () (make-instance 'array-list)))
   (test-add-before-empty #'(lambda () (make-instance 'array-list)))
   (test-add-after-empty #'(lambda () (make-instance 'array-list)))

))

(deftest test-array-list-x-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'array-list-x)))
   (test-forward-traversal #'(lambda () (make-instance 'array-list-x)))
   (test-backward-traversal #'(lambda () (make-instance 'array-list-x)))
   (test-remove-forward #'(lambda () (make-instance 'array-list-x)))
   (test-remove-backward #'(lambda () (make-instance 'array-list-x)))
   (test-remove-inside-out #'(lambda () (make-instance 'array-list-x)))
   (test-add-before-empty #'(lambda () (make-instance 'array-list-x)))
   (test-add-after-empty #'(lambda () (make-instance 'array-list-x)))

))

(deftest test-singly-linked-list-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'singly-linked-list)))
   (test-forward-traversal #'(lambda () (make-instance 'singly-linked-list)))
   (test-backward-traversal #'(lambda () (make-instance 'singly-linked-list)))
   (test-remove-forward #'(lambda () (make-instance 'singly-linked-list)))
   (test-remove-backward #'(lambda () (make-instance 'singly-linked-list)))
   (test-remove-inside-out #'(lambda () (make-instance 'singly-linked-list)))
   (test-add-before-empty #'(lambda () (make-instance 'singly-linked-list)))
   (test-add-after-empty #'(lambda () (make-instance 'singly-linked-list)))

))

(deftest test-singly-linked-list-x-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-forward-traversal #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-backward-traversal #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-remove-forward #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-remove-backward #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-remove-inside-out #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-add-before-empty #'(lambda () (make-instance 'singly-linked-list-x)))
   (test-add-after-empty #'(lambda () (make-instance 'singly-linked-list-x)))

))

(deftest test-doubly-linked-list-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'doubly-linked-list)))
   (test-forward-traversal #'(lambda () (make-instance 'doubly-linked-list)))
   (test-backward-traversal #'(lambda () (make-instance 'doubly-linked-list)))
   (test-remove-forward #'(lambda () (make-instance 'doubly-linked-list)))
   (test-remove-backward #'(lambda () (make-instance 'doubly-linked-list)))
   (test-remove-inside-out #'(lambda () (make-instance 'doubly-linked-list)))
   (test-add-before-empty #'(lambda () (make-instance 'doubly-linked-list)))
   (test-add-after-empty #'(lambda () (make-instance 'doubly-linked-list)))

))

(deftest test-hash-table-list-list-iterator ()
  (check
   (test-list-iterator-vs-iterator #'(lambda () (make-instance 'hash-table-list)))
   (test-forward-traversal #'(lambda () (make-instance 'hash-table-list)))
   (test-backward-traversal #'(lambda () (make-instance 'hash-table-list)))
   (test-remove-forward #'(lambda () (make-instance 'hash-table-list)))
   (test-remove-backward #'(lambda () (make-instance 'hash-table-list)))
   (test-remove-inside-out #'(lambda () (make-instance 'hash-table-list)))
   (test-add-before-empty #'(lambda () (make-instance 'hash-table-list)))
   (test-add-after-empty #'(lambda () (make-instance 'hash-table-list)))

))

(deftest test-list-iterator-all ()
  (check
   (test-array-list-list-iterator)
   (test-array-list-x-list-iterator)
   (test-singly-linked-list-list-iterator)
   (test-singly-linked-list-x-list-iterator)
   (test-doubly-linked-list-list-iterator)
   (test-hash-table-list-list-iterator)))
