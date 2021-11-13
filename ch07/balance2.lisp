;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               balance2.lisp
;;;;
;;;;   Started:            Sun Jul 18 18:49:36 2021
;;;;   Modifications:
;;;;
;;;;   Purpose: Exercise 1. Empty string not considered a balanced string.
;;;;
;;;;   This exercise changes the fundamental definition of a string of balanced brackets:
;;;;   1. The string "[]" is balanced.
;;;;   2. If A is balanced, then so is [A].
;;;;   3. If A and B are balanced, then so is AB.
;;;;
;;;;   However, the change to the code is trivial--simply weed out an initial empty string.
;;;;   The logic of how [ and ] are balanced for a non-empty string is identical...
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
(load "/home/slytobias/lisp/books/Concise/containers.lisp")
;(load "/home/slytobias/lisp/books/Concise/ch07/balance.lisp")

(defpackage :balance2
  (:use :common-lisp :test :containers)
  (:shadowing-import-from :containers :type :push :pop))

(in-package :balance2)

(defun recursive-balanced-stream (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((current ()
               (peek-char nil stream nil nil))
             (advance ()
               (read-char stream nil nil))
             (check-sequential ()
               (cond ((null (current)) t)
                     ((char= (current) #\[)
                      (advance)
                      (cond ((null (current)) nil)
                            ((check-nested)
                             (cond ((null (current)) nil)
                                   ((char= (current) #\])
                                    (advance)
                                    (check-sequential))
                                   (t nil)))
                            (t nil)))
                     (t nil)))
             (check-nested ()
               (cond ((null (current)) nil) ; Incomplete enclosing expression (Only on recursive calls.)
                     ((char= (current) #\[)
                      (advance)
                      (cond ((null (current)) nil)
                            ((check-nested)
                             (cond ((null (current)) nil)
                                   ((char= (current) #\])
                                    (advance)
                                    (check-nested))
                                   (t nil)))
                            (t nil)))
                     ((char= (current) #\]) t) ; End of parent enclosing expression
                     (t nil))))
      (if (null (current))
          nil
          (check-sequential)))) )

(defun check-balanced-iterative (s)
  (if (zerop (length s))
      nil
      (do ((se (make-instance 'b:string-enumerator :source s))
           (count 0))
          ((b:emptyp se) (zerop count))
        (case (b:current se)
          (#\[ (incf count))
          (#\] (decf count))
          (otherwise (return nil)))
        (b:advance se)
        (when (minusp count)
          (return nil)))) )
      
(defun check-balanced-iterative (s)
  (if (zerop (length s))
      nil
      (do ((i 0 (1+ i))
           (count 0))
          ((= i (length s)) (zerop count))
        (case (char s i)
          (#\[ (incf count))
          (#\] (decf count))
          (otherwise (return nil)))
        (when (minusp count)
          (return nil)))) )

(defun check-balanced-iterative (s)
  (if (zerop (length s))
      nil
      (loop with count = 0
            for ch across s
            never (minusp 0) ; This won't catch a final mismatched ]
            do (case ch
                 (#\[ (incf count))
                 (#\] (decf count))
                 (otherwise (return nil)))
            finally (return (zerop count)))) ); But this will.

(defun check-balanced-stack (s)
  (if (zerop (length s))
      nil
      (do ((se (make-instance 'b:string-enumerator :source s))
           (stack (make-instance 'linked-stack :type 'character)))
          ((b:emptyp se) (emptyp stack))
        (cond ((char= (b:current se) #\[) (push stack (b:current se)))
              ((and (char= (b:current se) #\]) (not (emptyp stack))) (pop stack))
              (t (return nil)))
        (b:advance se))))
  
(defun check-balanced-stack (s)
  (if (zerop (length s))
      nil
      (do ((se (make-instance 'b:string-enumerator :source s))
           (stack (make-instance 'linked-stack :type 'character)))
          ((b:emptyp se) (emptyp stack))
        (case (b:current se)
          (#\[ (push stack (b:current se)))
          (#\] (if (emptyp stack)
                   (return nil)
                   (pop stack)))
          (otherwise (return nil)))
        (b:advance se))))
  
(defun check-balanced-stack (s)
  (if (zerop (length s))
      nil
      (loop with stack = (make-instance 'linked-stack :type 'character)
            for ch across s
            if (char= ch #\[)
              do (push stack ch)
            else if (and (char= ch #\]) (not (emptyp stack)))
              do (pop stack)
            else
              do (return nil)
            end
            finally (return (emptyp stack)))) )

(defun check-balanced-stack (s)
  (if (zerop (length s))
      nil
      (loop with stack = (make-instance 'linked-stack :type 'character)
            for ch across s
            do (case ch
                 (#\[ (push stack ch))
                 (#\] (if (emptyp stack)
                          (return nil)
                          (pop stack)))
                 (otherwise (return nil)))
            finally (return (emptyp stack)))) )

;(test-balanced #'check-balanced-stack)

(deftest test-balanced (f)
  (check
   (not (funcall f ""))
   (not (funcall f "["))
   (not (funcall f "]"))
   (funcall f "[]")
   (not (funcall f "[[]"))
   (not (funcall f "[]]"))
   (not (funcall f "[]]fads"))
   (not (funcall f "[]fads"))
   (funcall f "[[]]")
   (not (funcall f "[a[]]"))
   (not (funcall f "[[]a]"))
   (not (funcall f "[[a]]"))
   (not (funcall f "[[]]a"))
   (funcall f "[[[]]]")
   (funcall f "[][]")
   (funcall f "[[][]]")
   (funcall f "[[]][[]]")
   (not (funcall f "[[]a[]]"))
   (funcall f "[[[][]][]]")
   (not (funcall f "[[[]][]][]]"))
   (funcall f "[][][[][[]]]")
   (funcall f "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]")
   (not (funcall f "[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]"))
   (not (funcall f "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]"))
   (not (funcall f "[[[[[[[[[[[][[[[]][]]]]]][[][]][][[[][]]][[][][]]]]][][[]][[][[[[]]]][[][]]][[]][[][[]][]][[]][[[[][][[[][][[[[[]][[[]][][[][[[[[[][][[]]][[[[][[][]][]][]]][][[[]]]]][][[[[]]]][]]][][[][][[[]]]][[[[[]][[[]]][[][]][]]][][]][[]]][]][[]][]][[[][][[][][]]][[][[[][]]][[[[][][][[[[[[]][[][[[[[[[[][[]][]][][[[]][[[][[][[]][[]][[][[]][]][][]]]]][[][[][]]][]][[[][[]]][][]][]][]][[[[]]][[[]]]][][][[[][[][][[[][[][[][]]]][][]][[]]]]]][[[[]][][[]]][]][][][][[][]][]][]][[]]][[][][[]][[][[][[]][[[]][[[[[[]][]]][[][[[]]]][][][[[][]]][]]][[[[]]]]]]][][]]][][[[][]]][[][[][[[[[]][]][[[[[]]]][[][]]][[]]]]][]][]][][[][[][[]]][]][]]]][[]][[]]][[[][][][][[]][][[[[]][]]][]]][][[[][[[[[[[]][][]]][[[][][[]]]][][][[[[][]][]]]][]]]]][]]]]][]]][]]][][[[[][][]]][]]]]]]]"))))
