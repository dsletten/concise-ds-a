;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               infix.lisp
;;;;
;;;;   Started:            Thu Jun 17 17:02:13 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Evaluate infix arithmetic expression
;;;;       Juxtapose recursive vs. stack-based implementations.
;;;;
;;;;       Simple case requires fully-parenthesized expression.
;;;;       
;;;;       More sophisticated (recursive descent) establishes precedence without parentheses.
;;;;       Parentheses still have highest precedence.
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
;;;;   TODO: Implement exponentiation (right-associative!) and unary +/- in EVAL-INFIX.
;;;;
;;;;
;(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Concise/containers.lisp")

(defpackage :infix
  (:use :common-lisp :test :containers)
  (:shadowing-import-from :containers :type :push :pop))

(in-package :infix)

(defun lookup (operator)
  (ecase operator
    ((+ - * /) (symbol-function operator))
    (% #'mod)))
  
(defun evaluate (operator op1 op2)
;  (print (list op1 operator op2))
  (funcall (lookup operator) op1 op2))

;;;
;;;    This is kind of a lucky accident. Won't work with other langs...
;;;    Treat fully-parenthesized expression as an (infix) S-expression.
;;;    
(defun lisp-eval-infix (s)
  "Evaluate a fully-parenthesized infix expression using built-in Lisp machinery."
  (labels ((eval-expression (expr)
             (cond ((null expr) (error "Missing argument"))
                   ((numberp expr) expr)
                   (t (destructuring-bind (op1 operator op2) expr
                        (evaluate operator (eval-expression op1) (eval-expression op2)))) )))
    (eval-expression (read-from-string s nil nil)))) ; Read the entire expression as a Lisp object.
                                        ; Numbers, symbols, nested subexpressions all instantiated.


;; ? (lisp-eval-infix "(10 / 5)")
;; 2
;; ? (lisp-eval-infix "(10 / -5)")
;; -2
;; ? (lisp-eval-infix "(9 * 8)")
;; 72
;; ? (lisp-eval-infix "((2 + 8) * (7 % 3))")
;; 10
;; ? (lisp-eval-infix "(((2 * 3) + 5) % 4)")
;; 3
;; ? (lisp-eval-infix "(((2 * 5) % (6 / 4)) + (2 * 3))")
;; 7
;; ? (lisp-eval-infix "(1 + (2 + (3 + 4)))")
;; 10
;; ? (lisp-eval-infix "(((1 + 2) + 3))")       ; Missing final operator/operand. Result is extra set of ()...
;; > Error: (((1 + 2) + 3)) can't be destructured against the lambda list (OP1 OPERATOR OP2), because it contains 1 elements, and exactly 3 are expected.
;; > While executing: CCL::PREPARE-TO-DESTRUCTURE, in process listener(1).
;; > Type :POP to abort, :R for a list of available restarts.
;; > Type :? for other options.
;; 1 > :a

;; ? (lisp-eval-infix "(((1 + 2) + 3) + 4)")
;; 10

;;;
;;;    Slightly different from prefix/postfix version. Must handle ().
;;;    
(defun read-token (stream)
  (case (peek-char t stream nil nil)
    ((#\( #\)) (intern (string (read-char stream))))
    (otherwise (read stream nil nil))))

;;;
;;;    Sedgewick Algorithms 4e pg. 129
;;;    Must be fully parenthesized.
;;;    
(defun stack-eval-infix (s)
  (let ((stream (make-string-input-stream s))
        (operator-stack (make-instance 'linked-stack :type 'symbol))
        (operand-stack (make-instance 'linked-stack :type 'number)))
    (do ((token (read-token stream) (read-token stream)))
        ((null token))
      (etypecase token
        (symbol (ecase token
                  (\() ; Ignore
                  (\) (let ((op (pop operator-stack))
                            (op2 (pop operand-stack))
                            (op1 (pop operand-stack)))
                        (push operand-stack (evaluate op op1 op2))))
                  ((+ - * / %) (push operator-stack token))))
        (number (push operand-stack token))))
    ;; (unless (null (read-token stream)) ; This doesn't fix the problem of trailing (!
    ;;   (error "Malformed expression"))
    (prog1 (cond ((emptyp operand-stack) (error "Missing expression"))
                 (t (pop operand-stack)))
      (unless (and (emptyp operand-stack)
                   (emptyp operator-stack))
        (error "Malformed expression")))) )
      
;;;
;;;    D'oh!
;;;    (stack-eval-infix "9 ((") => 9
;;;    These are fixed...
;;;    (stack-eval-infix "9 ((8") => 8
;;;    (stack-eval-infix "9 ((+") => 9
;;;    (stack-eval-infix "9 +") => 9

;;;
;;;    Immutable version -- inspired by Clojure.
;;;    
(defun stack-eval-infix (s)
  (labels ((evaluate-op (operator-stack operand-stack)
             (if (null operand-stack)
                 (error "Missing argument")
                 (let ((op2 (first operand-stack))
                       (operand-stack (rest operand-stack)))
                   (if (null operand-stack)
                       (error "Missing argument")
                       (let ((op1 (first operand-stack))
                             (operand-stack (rest operand-stack)))
                         (if (null operator-stack)
                             (error "Missing operator")
                             (let ((op (first operator-stack))
                                   (operator-stack (rest operator-stack)))
                               (values operator-stack (cons (evaluate op op1 op2) operand-stack)))) )))) )
           (eval-expression (tokens operator-stack operand-stack)
             (if (null tokens)
                 (cond ((null operand-stack) (error "Missing expression"))
                       (t (cond ((and (null (rest operand-stack))
                                      (null operator-stack))
                                 (first operand-stack))
                                (t (error "Malformed expression")))) )
                 (let ((token (first tokens)))
                   (etypecase token
                     (symbol (ecase token
                               (\( (eval-expression (rest tokens) operator-stack operand-stack)) ; Ignore
                               (\) (apply #'eval-expression (rest tokens) (multiple-value-list (evaluate-op operator-stack operand-stack))))
                               ((+ - * / %) (eval-expression (rest tokens) (cons token operator-stack) operand-stack))))
                     (number (eval-expression (rest tokens) operator-stack (cons token operand-stack)))) ))))
    (eval-expression (tokenize s) '() '())))

;;;
;;;    D'oh! The right way.
;;;    
(defun stack-eval-infix (s)
  (labels ((evaluate-op (operator-stack operand-stack)
             (if (emptyp operand-stack)
                 (error "Missing argument")
                 (let ((op2 (top operand-stack))
                       (operand-stack (pop operand-stack)))
                   (if (emptyp operand-stack)
                       (error "Missing argument")
                       (let ((op1 (top operand-stack))
                             (operand-stack (pop operand-stack)))
                         (if (emptyp operator-stack)
                             (error "Missing operator")
                             (let ((op (top operator-stack))
                                   (operator-stack (pop operator-stack)))
                               (values operator-stack (push operand-stack (evaluate op op1 op2)))) )))) ))
           (eval-expression (tokens operator-stack operand-stack)
             (if (null tokens)
                 (cond ((emptyp operand-stack) (error "Missing expression"))
                       (t (cond ((and (emptyp (pop operand-stack))
                                      (emptyp operator-stack))
                                 (top operand-stack))
                                (t (error "Malformed expression")))) )
                 (let ((token (first tokens)))
                   (etypecase token
                     (symbol (ecase token
                               (\( (eval-expression (rest tokens) operator-stack operand-stack)) ; Ignore
                               (\) (apply #'eval-expression (rest tokens) (multiple-value-list (evaluate-op operator-stack operand-stack))))
                               ((+ - * / %) (eval-expression (rest tokens) (push operator-stack token) operand-stack))))
                     (number (eval-expression (rest tokens) operator-stack (push operand-stack token)))) ))))
    (eval-expression (tokenize s) 
                     (make-instance 'persistent-stack :type 'symbol)
                     (make-instance 'persistent-stack :type 'number))))

;;;
;;;    Mutually recursive.
;;;    No need for multiple values. Both nested functions simply keep passing in necessary args to each other.
;;;    
(defun stack-eval-infix (s)
  (labels ((evaluate-op (tokens operator-stack operand-stack)
             (if (emptyp operand-stack)
                 (error "Missing argument")
                 (let ((op2 (top operand-stack))
                       (operand-stack (pop operand-stack)))
                   (if (emptyp operand-stack)
                       (error "Missing argument")
                       (let ((op1 (top operand-stack))
                             (operand-stack (pop operand-stack)))
                         (if (emptyp operator-stack)
                             (error "Missing operator")
                             (let ((op (top operator-stack))
                                   (operator-stack (pop operator-stack)))
                               (eval-expression tokens operator-stack (push operand-stack (evaluate op op1 op2)))) )))) ))
           (eval-expression (tokens operator-stack operand-stack)
             (if (null tokens)
                 (cond ((emptyp operand-stack) (error "Missing expression"))
                       (t (cond ((and (emptyp (pop operand-stack))
                                      (emptyp operator-stack))
                                 (top operand-stack))
                                (t (error "Malformed expression")))) )
                 (let ((token (first tokens)))
                   (etypecase token
                     (symbol (ecase token
                               (\( (eval-expression (rest tokens) operator-stack operand-stack)) ; Ignore
                               (\) (evaluate-op (rest tokens) operator-stack operand-stack))
                               ((+ - * / %) (eval-expression (rest tokens) (push operator-stack token) operand-stack))))
                     (number (eval-expression (rest tokens) operator-stack (push operand-stack token)))) ))))
    (eval-expression (tokenize s) 
                     (make-instance 'persistent-stack :type 'symbol)
                     (make-instance 'persistent-stack :type 'number))))

(deftest test-stack-eval-infix ()
  (check
   (= (stack-eval-infix "9") 9)
   (= (stack-eval-infix "9 ((") 9) ; This shold fail...
   (= (stack-eval-infix "    9    ") 9)
   (= (stack-eval-infix "(10 / 5)") 2)
   (= (stack-eval-infix "(10 / -5)") -2)
   (= (stack-eval-infix "(9 * 8)") 72)
   (= (stack-eval-infix "(2 + 3)") 5)
   (= (stack-eval-infix "(3 * -6)") -18)
   (= (stack-eval-infix "((4 + 5) * 9)") 81)
   (= (stack-eval-infix "((2 + 8) * (7 % 3))") 10)
   (= (stack-eval-infix "(((2 * 3) + 5) % 4)") 3)
   (= (stack-eval-infix "(((2 * 5) % (6 / 4)) + (2 * 3))") 7)
   (apply #'= 10 (mapcar #'stack-eval-infix '("(1 + (2 + (3 + 4)))" "(((1 + 2) + 3) + 4)")))
   (= (stack-eval-infix "((1 + 2) + 3)") 6)
   (= (stack-eval-infix "(99 - (7 * 13))") (- 99 (* 7 13))) ; Fox only handles single-digit numerals!
   (apply #'= (mapcar #'stack-eval-infix '("(2 + 3)" "((1 + 1) + 3)" "(((8 / 8) + 1) + 3)" "((((4 * 2) / 8) + 1) + 3)")))
   (apply #'= (mapcar #'stack-eval-infix '("(2 + 3)" "(2 + (2 + 1))" "(2 + (2 + (8 / 8)))" "(2 + (2 + (8 / (4 * 2))))")))) )

;;;
;;;    Art of Java - recursive descent parser. Relaxes requirement for parenthesization.
;;;    Precedence hard-wired into structure of recursive function calls.
;;;
;;;
;;;    EVAL-INFIX0, EVAL-INFIX1 are broken?!
;;;    
(defun eval-infix0 (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression ()
               (let ((token (read-token stream)))
                 (cond ((null token) (error "Missing argument"))
                       (t (eval-term token)))) )
             (eval-term (token)
               (let ((op1 (eval-factor token)))  ; let*??
                 (let ((operator (read-token stream)))
;                   (cond ((null operator) (error "Missing operator"))
                   (cond ((null operator) op1)
                         (t (let ((token (read-token stream)))
                              (let ((op2 (eval-factor token)))
                                (ecase operator
                                  ((+ -) (evaluate operator op1 op2)))) )))) ))
             (eval-factor (token)
               (let ((op1 (eval-parenthesized token)))  ; let*??
                 (let ((operator (read-token stream)))
;                   (cond ((null operator) (error "Missing argument"))
                   (cond ((null operator) op1)
                         (t (let ((token (read-token stream)))
                              (let ((op2 (eval-parenthesized token)))
                                (ecase operator
                                  ((* / %) (evaluate operator op1 op2)))) )))) ))
             (eval-parenthesized (token)
               (case token
                 (\( (let ((token (read-token stream)))
                       (cond ((null token) (error "Missing argument"))
                             (t (let ((result (eval-term token)))
                                  (let ((token (read-token stream)))
                                    (unless (eql token '\))
                                      (error "Missing delimiter"))
                                    result)))) ))
                 (otherwise (eval-atom token))))
             (eval-atom (token)
               (cond ((numberp token) token)
                     (t (error "Malformed atom: ~A" token)))) )
      (eval-expression))))

(defun eval-infix1 (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((eval-expression ()
               (let ((token (read-token stream)))
                 (cond ((null token) (error "Missing argument"))
                       (t (eval-term token)))) )
             (eval-term (token)
               (let ((op1 (eval-factor token)))
                 (do ((operator (read-token stream) (read-token stream))
;                      (result op1 (evaluate operator result (eval-parenthesized (read-token stream)))) )
                      (result op1))
                     ((not (member operator '(+ -))) result)
                   (setf result (evaluate operator result (eval-factor (read-token stream)))) )))
             (eval-factor (token)
               (let ((op1 (eval-parenthesized token)))  ; let*??
                 (do ((operator (read-token stream) (read-token stream))
                      (result op1))
                     ((not (member operator '(* / %))) result)
                   (setf result (evaluate operator result (eval-parenthesized (read-token stream)))) )))
             (eval-parenthesized (token)
               (case token
                 (\( (let ((token (read-token stream)))
                       (cond ((null token) (error "Missing argument"))
                             (t (let ((result (eval-term token)))
                                  (let ((token (read-token stream)))
                                    (unless (eql token '\))
                                      (error "Missing delimiter"))
                                    result)))) ))
                 (otherwise (eval-atom token))))
             (eval-atom (token)
               (cond ((numberp token) token)
                     (t (error "Malformed atom: ~A" token)))) )
      (eval-expression))))

;;;
;;;    Need list of tokens since we might need to pushback misinterpreted token.
;;;    Harder to do with a stream...
;;;    
(defun tokenize (s)
  "Break the input string S into a list of tokens."
  (loop with stream = (make-string-input-stream s)
        for token = (read-token stream) 
        until (null token) collect token))

;;;
;;;    TOKENS as parameter. VALUES needed to return result + remaining tokens.
;;;    Messed up DO semantics...
;;;    
(defun eval-infix (s)
  (labels ((eval-expression (tokens)
             (cond ((null tokens) (error "Missing argument"))
                   (t (values (eval-term (first tokens) (rest tokens)))) ))
           (eval-term (token tokens)
             (multiple-value-bind (op1 more) (eval-factor token tokens)
               (cond ((null more) (values op1 more))
                     (t (do ((operator (first more) (first tokens))
                             (tokens (rest more) (rest tokens))
;                      (result op1 (evaluate operator result (eval-parenthesized (read-token stream)))) )
                             (result op1))
                            ((not (member operator '(+ -))) (values result (cons operator tokens)))
                          (multiple-value-bind (op2 more-tokens) (eval-factor (first tokens) (rest tokens))
                            (setf result (evaluate operator result op2)
                                  tokens more-tokens)))) )))
           (eval-factor (token tokens)
             (multiple-value-bind (op1 more) (eval-parenthesized token tokens)
               (cond ((null more) (values op1 more))
                     (t (do ((operator (first more) (first tokens))
                             (tokens (rest more) (rest tokens))
                             (result op1))
                            ((not (member operator '(* / %))) (values result (cons operator tokens)))
                          (multiple-value-bind (op2 more-tokens) (eval-parenthesized (first tokens) (rest tokens))
                            (setf result (evaluate operator result op2)
                                  tokens more-tokens)))) )))
           (eval-parenthesized (token tokens)
             (case token
               (\( (cond ((null tokens) (error "Missing argument"))
                         (t (multiple-value-bind (result tokens) (eval-term (first tokens) (rest tokens))
                              (unless (eql (first tokens) '\))
                                (error "Missing delimiter"))
                              (values result (rest tokens)))) ))
               (otherwise (values (eval-atom token) tokens))))
           (eval-atom (token)
             (cond ((numberp token) token)
                   (t (error "Malformed atom: ~A" token)))) )
    (eval-expression (tokenize s))))

;;;
;;;    TOKENS as mutable variable
;;;    
(defun eval-infix* (s)
  (let ((tokens (tokenize s)))
    (labels ((eval-expression ()
               (cond ((null tokens) (error "Missing argument"))
                     (t (eval-term (cl:pop tokens)))) )
             (eval-term (token)
               (let ((op1 (eval-factor token)))
                 (cond ((null tokens) op1)
                       (t (do ((operator (cl:pop tokens) (cl:pop tokens))
                               (result op1))
                              ((not (member operator '(+ -))) (cl:push operator tokens) result)
                            (setf result (evaluate operator result (eval-factor (cl:pop tokens)))) )))) )
             (eval-factor (token)
               (let ((op1 (eval-parenthesized token)))
                 (cond ((null tokens) op1)
                       (t (do ((operator (cl:pop tokens) (cl:pop tokens))
                               (result op1))
                              ((not (member operator '(* / %))) (cl:push operator tokens) result)
                            (setf result (evaluate operator result (eval-parenthesized (cl:pop tokens)))) )))) )
             (eval-parenthesized (token)
               (case token
                 (\( (cond ((null tokens) (error "Missing argument"))
                           (t (let ((result (eval-term (cl:pop tokens))))
                                (unless (eql (cl:pop tokens) '\))
                                  (error "Missing delimiter"))
                                result))))
                 (otherwise (eval-atom token))))
             (eval-atom (token)
               (cond ((numberp token) token)
                     (t (error "Malformed atom: ~A" token)))) )
      (eval-expression))))

;;;
;;;    TOKENS as mutable variable
;;;    Aux functions vs. DO!
;;;    
(defun eval-infix* (s)
  (let ((tokens (tokenize s)))
    (labels ((eval-expression ()
               (cond ((null tokens) (error "Missing argument"))
                     (t (eval-term (cl:pop tokens)))) )
             (eval-term (token)
               (let ((op1 (eval-factor token)))
                 (cond ((null tokens) op1)
                       (t (eval-additive op1 (cl:pop tokens)))) ))
             (eval-additive (op1 operator)
               (case operator
                 ((+ -) (eval-additive (evaluate operator op1 (eval-factor (cl:pop tokens))) ; This is only safe since Common Lisp guarantees L-to-R order of arg evaluation.
                                       (cl:pop tokens)))                                     ;    In other words, this POP has to occur after EVAL-FACTOR above returns.
                 ((nil) (values op1 tokens))
                 (otherwise (cl:push operator tokens) op1)))
             (eval-factor (token)
               (let ((op1 (eval-parenthesized token)))
                 (cond ((null tokens) op1)
                       (t (eval-multiplicative op1 (cl:pop tokens)))) ))
             (eval-multiplicative (op1 operator)
               (case operator
                 ((* / %) (eval-multiplicative (evaluate operator op1 (eval-parenthesized (cl:pop tokens)))
                                               (cl:pop tokens)))
                 ((nil) (values op1 tokens))
                 (otherwise (cl:push operator tokens) op1)))
             (eval-parenthesized (token)
               (case token
;                 (\( (cond ((null tokens) (error "Missing argument"))
;                           (t (let ((result (eval-term (cl:pop tokens))))
                 (\( (let ((result (eval-expression)))
                       (unless (eql (cl:pop tokens) '\))
                         (error "Missing delimiter"))
                       result))
                 (otherwise (eval-atom token))))
             (eval-atom (token)
               (cond ((numberp token) token)
                     (t (error "Malformed atom: ~A" token)))) )
      (prog1 (eval-expression)
        (unless (null tokens)
          (error "Malformed expression. Remaining tokens: ~A" tokens)))) ))
  
;;;
;;;    TOKENS as parameter. VALUES needed to return result + remaining tokens.
;;;    Cleaned up gross DO semantics...
;;;
;;;    This is the one.
;;;    
(defun eval-infix (s)
  (labels ((eval-expression (tokens)
             (cond ((null tokens) (error "Missing expression"))
                   (t (eval-term (first tokens) (rest tokens)))) )
           (eval-term (token tokens)
             (multiple-value-bind (op1 more) (eval-factor token tokens)
               (cond ((null more) (values op1 '()))
                     (t (eval-additive op1 (first more) (rest more)))) ))
           (eval-additive (op1 operator tokens)
             (case operator
               ((+ -) (if (null tokens)
                          (error "Missing argument to ~A" operator)
                          (multiple-value-bind (op2 more) (eval-factor (first tokens) (rest tokens))
                            (if (null more)
                                (values (evaluate operator op1 op2) '())
                                (eval-additive (evaluate operator op1 op2) (first more) (rest more)))) ))
               (otherwise (values op1 (cons operator tokens)))) ) ; Push back token we shouldn't have consumed. Only ) possible here??
           (eval-factor (token tokens)
             (multiple-value-bind (op1 more) (eval-parenthesized token tokens)
               (cond ((null more) (values op1 '()))
                     (t (eval-multiplicative op1 (first more) (rest more)))) ))
           (eval-multiplicative (op1 operator tokens)
             (case operator
               ((* / %) (if (null tokens)
                            (error "Missing argument to ~A" operator)
                            (multiple-value-bind (op2 more) (eval-parenthesized (first tokens) (rest tokens))
                              (if (null more)
                                  (values (evaluate operator op1 op2) '())
                                  (eval-multiplicative (evaluate operator op1 op2) (first more) (rest more)))) ))
               (otherwise (values op1 (cons operator tokens)))) ) ; Push back token we shouldn't have consumed.
           (eval-parenthesized (token tokens)
             (case token
               (\( (multiple-value-bind (result more) (eval-expression tokens)
                     (unless (eql (first more) '\))
                       (error "Missing delimiter"))
                     (values result (rest more))))
               (otherwise (values (eval-atom token) tokens))))
           (eval-atom (token)
             (cond ((numberp token) token)
                   (t (error "Malformed atom: ~A" token)))) )
      (multiple-value-bind (result tokens) (eval-expression (tokenize s))
        (unless (null tokens)
          (error "Malformed expression. Remaining tokens: ~A" tokens))
        result)))

;;;
;;;    This is so brittle...can only unread one char from the stream.
;;;    - Have to be careful with unary/binary '-':
;;;      - Binary '-' is an operator. Converted to symbol. May need to pushback.
;;;      - Unary '-' should really be part of the number token that follows it.
;;;      
(defun eval-infix-stream (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((read-token ()
               (case (peek-char t stream nil nil)
                 (#\- (let ((ch (read-char stream))
                            (ch1 (peek-char nil stream nil nil)))
                        (cond ((null ch1) (error "End of the road, buddy..."))
                              ((digit-char-p ch1) (- (read stream nil nil))) ; It's really a number
                              (t (intern (string ch)))) )) ; It's really an operator
                 ((#\( #\) #\+ #\* #\/ #\%) (intern (string (read-char stream))))
                 (otherwise (read stream nil nil))))
             (push-back (token)
               (unread-char (char-downcase (char (symbol-name token) 0)) stream)) ; Downcase needed for alpha chars...but they aren't recognized anyway. Just moving the error.
             (eval-expression ()
               (let ((token (read-token)))
                 (cond ((null token) (error "Missing expression"))
                       (t (eval-term token)))) )
             (eval-term (token)
               (let ((op1 (eval-factor token)))
                 (let ((token (read-token)))
                   (cond ((null token) op1)
                         (t (eval-additive op1 token)))) ))
             (eval-additive (op1 operator)
               (case operator
                 ((+ -) (let ((token (read-token)))
                          (if (null token)
                              (error "Missing argument to ~A" operator)
                              (let ((op2 (eval-factor token)))
                                (let ((token (read-token)))
                                  (if (null token)
                                      (evaluate operator op1 op2)
                                      (eval-additive (evaluate operator op1 op2) token)))) )))
                 (otherwise (push-back operator) op1)))
             (eval-factor (token)
               (let ((op1 (eval-parenthesized token)))
                 (let ((token (read-token)))
                   (cond ((null token) op1)
                         (t (eval-multiplicative op1 token)))) ))
             (eval-multiplicative (op1 operator)
               (case operator
                 ((* / %) (let ((token (read-token)))
                            (if (null token)
                                (error "Missing argument to ~A" operator)
                                (let ((op2 (eval-parenthesized token)))
                                  (let ((token (read-token)))
                                    (if (null token)
                                        (evaluate operator op1 op2)
                                        (eval-multiplicative (evaluate operator op1 op2) token)))) )))
                 (otherwise (push-back operator) op1)))
             (eval-parenthesized (token)
               (case token
                 (\( (prog1 (eval-expression)
                       (let ((token (read-token)))
                         (unless (eql token '\))
                           (error "Missing delimiter")))) )
                 (otherwise (eval-atom token))))
             (eval-atom (token)
               (cond ((numberp token) token)
                     (t (error "Malformed atom: ~A" token)))) )
      (prog1 (eval-expression)
        (let ((token (read-token)))
          (unless (null token)
            (error "Malformed expression. Remaining tokens: ~A" token)))) )))

(deftest test-infix (f)
  (check
   (= (funcall f "9") 9)
   (= (funcall f "(9)") 9) ; Superfluous ()
   (= (funcall f "    9    ") 9)
   (= (funcall f "(10 / 5)") 2)
   (= (funcall f "10 / 5") 2)
   (= (funcall f "(10 / -5)") -2)
   (= (funcall f "10 / -5") -2)
   (= (funcall f "(9 * 8)") 72)
   (= (funcall f "9 * 8") 72)
   (= (funcall f "2 + 3") 5)
   (= (funcall f "3 * -6") -18)
   (= (funcall f "(4 + 5) * 9") 81)
   (= (funcall f "((2 + 8) * (7 % 3))") 10)
   (= (funcall f "(2 + 8) * (7 % 3)") 10)
   (= (funcall f "(((2 * 3) + 5) % 4)") 3)
   (= (funcall f "(2 * 3 + 5) % 4") 3)
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
   (= (funcall f "(((2 * 5) % (6 / 4)) + (2 * 3))") 7)
   (= (funcall f "(2 * 5 % (6 / 4)) + 2 * 3") 7)
   (apply #'= 10 (mapcar f '("(1 + (2 + (3 + 4)))" "(((1 + 2) + 3) + 4)" "1 + (2 + (3 + 4))" "((1 + 2) + 3) + 4" "1 + 2 + 3 + 4")))
   (= (funcall f "(((1 + 2) + 3))") 6) ; Extra ()
   (= (funcall f "99 - 7 * 13") (- 99 (* 7 13))) ; Fox only handles single-digit numerals!
   (apply #'= (mapcar f '("2 + 3" "(1 + 1) + 3" "(8 / 8 + 1) + 3" "4 * 2 / 8 + 1 + 3")))
   (apply #'= (mapcar f '("2 + 3" "2 + (2 + 1)" "2 + (2 + 8 / 8)" "2 + (2 + 8 / (4 * 2))")))) )
