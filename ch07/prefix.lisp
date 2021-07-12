;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               prefix.lisp
;;;;
;;;;   Started:            Wed May 19 14:44:52 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate (unparenthesized) prefix arithmetic expression
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

(defpackage :prefix
  (:use :common-lisp :lang :test :containers)
  (:shadowing-import-from :containers :type :push :pop))

(in-package :prefix)

(defun lookup (operator)
  (ecase operator
    ((+ - * /) (symbol-function operator))
    (% #'mod)))
  
(defun evaluate (operator op1 op2)
  (funcall (lookup operator) op1 op2))

(defun read-token (stream)
  (read stream nil nil))

;;;
;;;    Simple recursive implementation.
;;;    
(defun eval-prefix (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression ()
               (let ((token (read-token stream)))
                 (cond ((null token) (error "Missing argument"))
                       ((numberp token) token)
                       (t (evaluate token (eval-expression) (eval-expression)))) )))
      (prog1 (eval-expression)
        (assert (null (read-token stream)))) )))

;;;
;;;    Trying out IF-LET
;;;    
(defun eval-prefix (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression ()
               (if-let (token (read-token stream))
                   (if (numberp token)
                       token
                       (evaluate token (eval-expression) (eval-expression)))
                 (error "Missing argument"))))
      (prog1 (eval-expression)
        (assert (null (read-token stream)))) )))

;;;
;;;    Every prefix expression consists of either:
;;;    1. A number
;;;    2. A binary operator followed by 2 prefix expressions
;;;
;;;    Thus, leaves are all numbers and at least one non-terminal node has 2 numeric child terminal nodes. (So? Prove?)
;;;    

;;;
;;;    Each operator is either followed by:
;;;    a. another operator - (compund) left operand must be computed first (and then still unseen right operand) before preceding operator can be evaluated.
;;;       or
;;;    b. the left operand - Push MARKER onto operator stack. Push left operand onto stack. Operator under MARKER is later applied to left operand.
;;;                          (Operator stack missing MARKER prior to this since previous operator was last pushed. MARKER now indicates that operand is L operand.)
;;;                          This is also the special case of the expression entirely consisting of a number.
;;;
;;;    Left operand is followed by:
;;;    a. another operator - (compound) right operand must be computed first before preceding operator can be evaluated.
;;;       or
;;;    b. the right operand - MARKER on top of operator stack. Actual operator below it.
;;;                           (MARKER on top of operator stack indicates that L operand has already been computed.)
;;;    
(defconstant marker :v)

(defun mark (stack)
  (push stack marker))

(defun markedp (stack)
  (and (not (emptyp stack)) (eq (top stack) marker)))

(defun stack-eval-prefix (s)
  (assert (not (zerop (length (string-trim " " s)))) )
  (let ((stream (make-string-input-stream s))
        (operator-stack (make-instance 'linked-stack :type 'symbol))
        (operand-stack (make-instance 'linked-stack :type 'number)))
    (do ((token (read-token stream) (read-token stream)))
        ((null token))
      (etypecase token
        (symbol (ecase token
                  ((+ - * / %) (push operator-stack token))))
        (number (do ((operand token))
                    ((not (markedp operator-stack))
                     (mark operator-stack)
                     (push operand-stack operand))
                  (pop operator-stack)
                  (when (emptyp operator-stack)
                    (error "Missing operator"))
                  (setf operand (evaluate (pop operator-stack) (pop operand-stack) operand)))) ))
    (cond ((emptyp operator-stack) (error "Missing argument")) ; Operator stack should consist of only marker.
          ((not (markedp operator-stack)) (error "Illegal state")) ; Not strictly necessary? Next clause would detect.
          ((emptyp operand-stack) (error "Missing expression")) ; Final value should be on operand stack.
          (t (prog1 (pop operand-stack)
               (pop operator-stack)
               (unless (emptyp operand-stack)
                 (error "Too many arguments"))
               (unless (emptyp operator-stack)
                 (error "Missing argument")))) )))

;;;
;;;    Clean up Fox's implementation...
;;;    
(defun stack-eval-prefix (s)
  (assert (not (zerop (length (string-trim " " s)))) )
  (let ((stream (make-string-input-stream s))
        (operator-stack (make-instance 'linked-stack :type 'symbol))
        (operand-stack (make-instance 'linked-stack :type 'number)))
    (labels ((process (operand)
               (if (not (markedp operator-stack))
                   (process-left-operand operand)
                   (process-right-operand operand)))
             (process-left-operand (op)
               (mark operator-stack)
               (push operand-stack op))
             (process-right-operand (op)
               (pop operator-stack)
               (when (emptyp operator-stack)
                 (error "Missing operator"))
               (process (evaluate (pop operator-stack) (pop operand-stack) op))))
      (do ((token (read-token stream) (read-token stream)))
          ((null token))
        (etypecase token
          (symbol (ecase token
                    ((+ - * / %) (push operator-stack token))))
          (number (process token))))
      (cond ((emptyp operator-stack) (error "Missing argument")) ; Operator stack should consist of only marker.
            ((not (markedp operator-stack)) (error "Illegal state")) ; Not strictly necessary? Next clause would detect.
            ((emptyp operand-stack) (error "Missing expression")) ; Final value should be on operand stack.
            (t (prog1 (pop operand-stack)
                 (pop operator-stack)
                 (unless (emptyp operand-stack)
                   (error "Too many arguments"))
                 (unless (emptyp operator-stack)
                   (error "Missing argument")))) ))))

(deftest test-prefix (f)
  (check
   (= (funcall f "9") 9)
   (= (funcall f "    9    ") 9)
   (= (funcall f "+ 2 3") 5)
   (= (funcall f "* 3 -6") -18)
   (= (funcall f "* + 4 5 9") 81)
   (= (funcall f "* + 2 8 % 7 3") 10)
   (= (funcall f "% + * 2 3 5 4") 3)
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
   (= (funcall f "+ % * 2 5 / 6 4 * 2 3") 7)
   (= (funcall f "+ 1 + 2 + 3 4") (funcall f "+ + + 1 2 3 4") 10)
   (= (funcall f "- 99 * 7 13") (- 99 (* 7 13))) ; Fox only handles single-digit numerals!
   (apply #'= (mapcar f '("+ 2 3" "+ + 1 1 3" "+ + / 8 8 1 3" "+ + / * 4 2 8 1 3")))
   (apply #'= (mapcar f '("+ 2 3" "+ 2 + 2 1" "+ 2 + 2 / 8 8" "+ 2 + 2 / 8 * 4 2")))) )

   
