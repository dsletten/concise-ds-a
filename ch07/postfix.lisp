;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               postfix.lisp
;;;;
;;;;   Started:            Thu Jun 10 03:56:25 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate (unparenthesized) postfix arithmetic expression
;;;;       Juxtapose recursive vs. stack-based implementations.
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Concise/containers.lisp")

(defpackage :postfix
  (:use :common-lisp :lang :test :containers)
  (:shadowing-import-from :containers :type :push :pop))

(in-package :postfix)

(defun lookup (operator)
  (ecase operator
    ((+ - * /) (symbol-function operator))
    (% #'mod)))
  
(defun evaluate (operator op1 op2)
  (funcall (lookup operator) op1 op2))

(defun read-token (stream)
  (read stream nil nil))

;;;
;;;    Recursive version
;;;    
(defun eval-postfix (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression-start ()
               (let ((token (read-token stream)))
                 (cond ((null token) (error "Missing argument"))
                       ((numberp token) (eval-expression-1 token))
                       (t (error "Malformed postfix expression.")))) )
             (eval-expression-1 (op1)
               (let ((token (read-token stream)))
                 (cond ((null token) op1)
                       ((numberp token) (eval-expression-1 (eval-expression-2 op1 token)))
                       (t (error "Malformed postfix expression.")))) )
             (eval-expression-2 (op1 op2)
               (let ((token (read-token stream)))
                 (etypecase token
                   (null (error "Missing argument"))
                   (symbol (ecase token
                             ((+ - * / %) (evaluate token op1 op2))))
                   (number (eval-expression-2 op1 (eval-expression-2 op2 token)))) )))
      (eval-expression-start))))
      ;; (prog1 (eval-expression-start)
      ;;   (assert (null (read-token stream)))) ))) ; No way to exit above without exhausting stream...

;;;
;;;    Trying out IF-LET
;;;    
(defun eval-postfix (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression-start ()
               (if-let (token (read-token stream))
                   (if (numberp token)
                       (eval-expression-1 token)
                       (error "Malformed postfix expression."))
                 (error "Missing argument")))
             (eval-expression-1 (op1)
               (if-let (token (read-token stream))
                   (if (numberp token)
                       (eval-expression-1 (eval-expression-2 op1 token))
                       (error "Malformed postfix expression."))
                 op1))
             (eval-expression-2 (op1 op2)
               (if-let (token (read-token stream))
                 (etypecase token
                   (symbol (ecase token
                             ((+ - * / %) (evaluate token op1 op2))))
                   (number (eval-expression-2 op1 (eval-expression-2 op2 token))))
                 (error "Missing argument"))))
      (eval-expression-start))))

;;;
;;;    Stack-based version
;;;    
(defun stack-eval-postfix (s)
;  (assert (not (zerop (length (string-trim " " s)))) )
  (let ((stream (make-string-input-stream s))
        (stack (make-instance 'linked-stack :type 'number)))
    (do ((token (read-token stream) (read-token stream)))
        ((null token))
      (etypecase token
        (symbol (ecase token
                  ((+ - * / %) 
                   (when (emptyp stack) (error "Missing argument"))
                   (let ((right (pop stack)))
                     (when (emptyp stack) (error "Missing argument"))
                     (let ((left (pop stack)))
                       (push stack (evaluate token left right)))) )))
        (number (push stack token))))
    (when (emptyp stack) (error "Missing expression"))
    (prog1 (pop stack)
      (unless (emptyp stack)
        (error "Too many arguments")))) )

(deftest test-postfix (f)
  (check
   (= (funcall f "9") 9)
   (= (funcall f "    9    ") 9)
   (= (funcall f "2 3 -") -1)
   (= (funcall f "3 2 -") 1) ; Sheesh, Nathan!
   (= (funcall f "2 3 +") 5)
   (= (funcall f "3 -6 *") -18)
   (= (funcall f "4 5 + 9 *") 81)
   (= (funcall f "2 8 + 7 3 % *") 10)
   (= (funcall f "2 3 * 5 + 4 %") 3)
   ;;
   ;;    The following test is iffy. It makes perfect sense in Lisp,
   ;;    but it is probably not the same result as in Ruby/Java.
   ;;    The result of evaluating the first 3 operators (mod (* 2 5) (/ 6 4)) is:
   ;;    (mod 10 6/4) => 1
   ;;    Since
   ;;    (floor 10 6/4) => 6; 1
   ;;    But in Ruby/Java, 6/4 => 1
   ;;    Equivalently:
   ;;    (mod 10 (truncate 6 4)) => 0
   ;;    Since
   ;;    (truncate 6 4) => 1; 2
   ;;    
   (= (funcall f "2 5 * 6 4 / % 2 3 * +") 7)
   (= (funcall f "1 2 3 4 + + +") (funcall f "1 2 + 3 + 4 +") 10)
   (= (funcall f "99 7 13 * -") (- 99 (* 7 13))) ; Fox only handles single-digit numerals!
   (apply #'= (mapcar f '("2 3 +" "1 1 + 3 +" "8 8 / 1 + 3 +" "4 2 * 8 / 1 + 3 +")))
   (apply #'= (mapcar f '("2 3 +" "2 2 1 + +" "2 2 8 8 / + +" "2 2 8 4 2 * / + +")))) )
