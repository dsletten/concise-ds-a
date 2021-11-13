;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               balance.lisp
;;;;
;;;;   Started:            Tue Apr  6 19:54:49 2021
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
;;;;   Wow! Fox was supposed to be making a point about the equivalence between recursion and using a stack when implementing an algorithm.
;;;;   But a funny thing happened on the way to the Forum...
;;;;   First, the stack implementation is utterly trivial. Encounter a '['? Push it on the stack. Encounter a ']'? Pop the stack. The expression
;;;;   is balanced unless:
;;;;   - Empty stack is popped (Mismatched ']')
;;;;   - Anything is left on stack at the end (Mismatched '[')
;;;;
;;;;   (Note that this is simpler than validating XML, where overlapping must be considered: <a> ... <b>...</a> ... </b> Illegal)
;;;;
;;;;   The second problem is that Fox's recursive example is fatally convoluted. Whether or not his code works, it is impossible to understand or
;;;;   to compare to the stack implementation.
;;;;
;;;;   I knew that there had to be a symmetric recursive implementation based on the fundamental definition:
;;;;   An expression is balanced if
;;;;   - It is the empty string
;;;;   - An expression s is balanced then '[s]' is balanced (Nesting).
;;;;   - If s and t are balanced, then st is balanced (Sequential concatenation).
;;;;
;;;;   But as I analyzed and refactored Fox's code I just couldn't find the symmetry. I wound up looking instead at recursive 'start' '[' and 'end' ']'
;;;;   functions that got a little closer, but it still wasn't what I wanted.
;;;;
;;;;   Finally, after many false starts I got it. Below are versions that use the STRING-ENUMERATOR class that I implemented to follow Fox as well as a
;;;;   more conventional Lisp string input stream. (Fox's StringEnumerator class returns nil when empty rather than throwing an exception.)
;;;;
;;;;   I feel satisfied after discovering the symmetry I expected. But at the same time it was very frustrating, mostly based on trial and error. I wish
;;;;   that I had some more theory to point me in the right direction.
;;;;
;;;;   See Advent of Code 2015 day 1.
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/Concise/containers.lisp")

(defpackage :balance
  (:nicknames :b)
  (:use :common-lisp :test :containers)
  (:export :string-enumerator :emptyp :current :advance :reset :duplicate)
  (:shadowing-import-from :containers :type :push :pop)
  (:shadow :emptyp))

(in-package :balance)

;; (defun balancedp (s)
;;   (labels ((check-balance (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) t)
;;                      ((char/= ch #\[) nil)
;;                      (t (peek-char nil stream nil(when (char=


;; (defun balancedp (s)
;;   (labels ((check-balanced (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) t)
;;                      ((char= ch #\[) (and (check-nested stream) (check-balanced stream)))
;;                      (t nil))))
;;            (check-nested (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) nil)
;;                      ((char= ch #\]) t)
;;                      ((char= ch #\[) (check-nested stream))
;;                      (t nil)))) )
;;     (check-balanced (make-string-input-stream s))))

;; (defun balancedp (s)
;;   (labels ((check-balanced (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) t)
;;                      ((char= ch #\[) (check-nested stream))
;;                      (t nil))))
;;            (check-nested (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) nil)
;;                      ((char= ch #\]) (check-balanced stream))
;;                      ((char= ch #\[) (check-nested stream))
;;                      (t nil)))) )
;;     (check-balanced (make-string-input-stream s))))

   
;;;
;;;    This works, but it seems like cheating...
;;;    
(defun validp (s)
  (cond ((string= s "") t)
        ((and (char= (char s 0) #\[)
              (char= (char s (1- (length s))) #\])  ; Not FSM. Has knowledge about end of string!
              (validp (subseq s 1 (1- (length s)))) ))
        (t (loop for i from 2 below (length s)
                 when (and (validp (subseq s 0 i))
                           (validp (subseq s i)))
                 do (return t)))) )

(deftest test-validp ()
  (check
   (validp "")
   (validp "[]")
   (validp "[[]]")
   (validp "[[[]]]")
   (validp "[][]")
   (validp "[[][]]")
   (validp "[[[][]][]]")
   (not (validp "[[[]][]][]]"))
   (validp "[][][[][[]]]")))

;; (defun balancedp (s)
;;   (labels ((start (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) nil)
;;                      ((char= ch #\[) (start stream))
;;                      ((char= ch #\]) t)
;;                      (t nil)))) )
;;     (let* ((stream (make-string-input-stream s))
;;            (ch (read-char stream nil nil)))
;;       (cond ((null ch) t)
;;             ((char= ch #\[) (start stream))
;;             (t nil)))) )

;; (defun balancedp (s)
;;   (labels ((sequential (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) t)
;;                      ((char= ch #\[) (and (nested stream) (sequential stream)))
;;                      (t nil))))
;;            (nested (stream)
;;              (let ((ch (read-char stream nil nil)))
;;                (cond ((null ch) nil)
;;                      ((char= ch #\[) (nested stream))
;; ;                     ((char= ch #\]) (sequential stream))
;;                      ((char= ch #\]) t)
;;                      (t nil)))) )
;;     (sequential (make-string-input-stream s))))

;; (deftest test-balancedp ()
;;   (check
;;    (balancedp "")
;;    (balancedp "[]")
;;    (not (balancedp "[[]"))
;;    (not (balancedp "[]]"))
;;    (not (balancedp "[]]fads"))
;;    (balancedp "[[]]")
;;    (balancedp "[[[]]]")
;;    (balancedp "[][]")
;;    (balancedp "[[][]]")
;;    (balancedp "[[[][]][]]")
;;    (not (balancedp "[[[]][]][]]"))
;;    (balancedp "[][][[][[]]]")))

(defclass string-enumerator ()
  ((s :initarg :source)
   (index :initform 0))
  (:documentation "Encapsulates a string as a stream of characters. Similar to string input stream."))

(defmethod print-object ((se string-enumerator) stream)
  (with-slots (s index) se
    (print-unreadable-object (se stream :type t)
      (format stream "|~A|" (subseq s index)))) )

(defgeneric emptyp (string-enumerator)
  (:documentation "Has the stream been fully consumed?"))
(defmethod emptyp ((se string-enumerator))
  (with-slots (s index) se
    (= index (length s))))

(defgeneric current (string-enumerator)
  (:documentation "Peek at current character in stream."))
(defmethod current :around ((se string-enumerator))
  (if (emptyp se)
      (error "String enumerator is empty.")
      (call-next-method)))
(defmethod current ((se string-enumerator))
  (with-slots (s index) se
    (char s index)))

(defgeneric advance (string-enumerator)
  (:documentation "Advance the stream to the next character."))
(defmethod advance :around ((se string-enumerator))
  (if (emptyp se)
      (error "String enumerator is empty.")
      (call-next-method)))
(defmethod advance ((se string-enumerator))
  (with-slots (index) se
    (prog1 (current se)
      (incf index))))

(defgeneric reset (string-enumerator)
  (:documentation "Reset the index to beginning of string."))
(defmethod reset ((se string-enumerator))
  (with-slots (index) se
    (setf index 0)))

(defgeneric duplicate (string-enumerator)
  (:documentation "Make a copy of the current state of this string enumerator."))
(defmethod duplicate ((se string-enumerator))
  (with-slots ((i0 index) s) se
    (let ((copy (make-instance 'string-enumerator :source s)))
      (with-slots ((i1 index)) copy
        (setf i1 i0))
      copy)))

;;;
;;;    Fox's Ruby code. Yuck
;;;    
;; def recursive_balanced?(string)
;;   source = StringEnumerator.new(string)
;;   check_balanced?(source) && source.empty?
;; end

;; def check_balanced?(source)
;;   return true if source.empty?
;;   return false unless source.current == "["
;;   source.next
;;   if source.current == "["
;;     return false unless check_balanced?(source)
;;   end
;;   return false unless source.current == "]"
;;   source.next
;;   return check_balanced?(source) if source.current == "["
;;   true
;; end

(defun recursive-balanced (s)
  (let ((se (make-instance 'string-enumerator :source s)))
    (and (check-balanced se)
         (emptyp se))))

;;;
;;;    Literal translation
;;;    
(defun check-balanced (se)
  (when (emptyp se)
    (return-from check-balanced t))
  (unless (char= (current se) #\[)
    (return-from check-balanced nil))
  (advance se)
  (when (emptyp se)
    (return-from check-balanced nil))
  (when (char= (current se) #\[)
    (unless (check-balanced se)
      (return-from check-balanced nil)))
; Returned from recursive call
  (when (emptyp se)
    (return-from check-balanced nil))
  (unless (char= (current se) #\])
    (return-from check-balanced nil))
  (advance se)
  (when (emptyp se)
    (return-from check-balanced t))
  (if (char= (current se) #\[)
      (check-balanced se)
      t))

;;;
;;;    Refactoring...
;;;    
(defun check-balanced (se)
  (cond ((emptyp se) t)
        ((char/= (current se) #\[) nil)
        (t (advance se)
           (cond ((emptyp se) nil) ; ?!?!?
                 ((and (char= (current se) #\[) (not (check-balanced se))) nil)
                                        ; Returned from recursive call
                 ((emptyp se) nil) ; ?!?!?!
                 ((char/= (current se) #\]) nil)
                 (t (advance se)
                    (cond ((emptyp se) t)
                          ((char= (current se) #\[) (check-balanced se))
                          (t t)))) )))
      
(defun check-balanced (se)
  (cond ((emptyp se) t)
        ((char= (current se) #\[)
         (advance se)
         (cond ((emptyp se) nil) ; ?!?!?
               ((and (char= (current se) #\[) (not (check-balanced se))) nil)
               ;; Returned from recursive call
               ((emptyp se) nil) ; ?!?!?!
               ((char= (current se) #\])
                (advance se)
                (cond ((emptyp se) t)
                      ((char= (current se) #\[) (check-balanced se))
                      (t t)))
               (t nil)))
        (t nil)))
      
;;;
;;;    Reference. This one works and is pretty clear.
;;;    
(defun check-balanced (se)
  (cond ((emptyp se) t)
        ((char= (current se) #\[)
         (advance se)
         (cond ((emptyp se) nil)
               ((or (char/= (current se) #\[) 
                    (and (check-balanced se) (not (emptyp se))))
                (cond ((char= (current se) #\])
                       (advance se)
                       (cond ((emptyp se) t)
                             ((char= (current se) #\[) (check-balanced se))
                             (t t))) ; This is a provisional T. The nested expression so far is balanced...
                      (t nil)))
               (t nil)))
        (t nil)))

;;;
;;;    Duplicated code
;;;    
(defun check-balanced (se)
  (cond ((emptyp se) t)
        ((char= (current se) #\[)
         (advance se)
         (cond ((emptyp se) nil)
               ((char= (current se) #\[) 
                (cond ((check-balanced se)
                       (cond ((emptyp se) nil)
                             ((char= (current se) #\])
                              (advance se)
                              (cond ((emptyp se) t)
                                    ((char= (current se) #\[) (check-balanced se))
                                    (t t))) ; Provisional
                             (t nil)))
                      (t nil)))
               ((char= (current se) #\])
                (advance se)
                (cond ((emptyp se) t)
                      ((char= (current se) #\[) (check-balanced se))
                      (t t))) ; Provisional
               (t nil)))
        (t nil)))

;;;
;;;    Final version! (Uh...see below)
;;;    
(defun check-balanced (se)
  (labels ((check-start (se)
             (cond ((check-balanced se)
                    (cond ((emptyp se) nil)
                          ((char= (current se) #\]) (check-end se))
                          (t nil)))
                   (t nil)))
           (check-end (se)
             (advance se)
             (cond ((emptyp se) t)
                   ((char= (current se) #\[) (check-balanced se))
                   ((char= (current se) #\]) t)
                   (t nil))))
    (cond ((emptyp se) t)
          ((char= (current se) #\[)
           (advance se)
           (cond ((emptyp se) nil)
                 ((char= (current se) #\[) (check-start se))
                 ((char= (current se) #\]) (check-end se))
                 (t nil)))
          (t nil))))

;;;
;;;    Final final version! Consolidated top-level function.
;;;    
(defun recursive-balanced (s)
  (let ((se (make-instance 'string-enumerator :source s)))
    (labels ((check-balanced ()
               (cond ((emptyp se) t)
                     ((char= (current se) #\[)
                      (advance se)
                      (cond ((emptyp se) nil)
                            ((char= (current se) #\[) (check-start))
                            ((char= (current se) #\]) (check-end))
                            (t nil)))
                     (t nil)))
             (check-start ()
               (cond ((check-balanced)
                      (cond ((emptyp se) nil)
                            ((char= (current se) #\]) (check-end))
                            (t nil)))
                     (t nil)))
             (check-end ()
               (advance se)
               (cond ((emptyp se) t)
                     ((char= (current se) #\[) (check-balanced))
                     ((char= (current se) #\]) t)
                     (t nil))))
      (and (check-balanced)
           (emptyp se)))) )

;;;
;;;    This one works.
;;;    
;; (defun recursive-balanced (s)
;;   (let ((se (make-instance 'string-enumerator :source s)))
;;     (labels ((check-sequential ()
;;                (cond ((emptyp se) t)
;;                      ((char= (current se) #\[)
;;                       (advance se)
;;                       (cond ((emptyp se) nil)
;;                             (t (if (check-nested)
;;                                    (cond ((emptyp se) nil)
;;                                          ((char= (current se) #\])
;;                                           (advance se)
;;                                           (check-sequential))
;;                                          (t nil))
;;                                    nil))))
;;                      (t nil)))
;;              (check-nested () ; Never called with empty SE
;;                (cond ((char= (current se) #\[)
;;                       (advance se)
;;                       (if (check-nested-sequential)
;;                           (if (emptyp se)
;;                               nil ; Incomplete enclosing expression
;;                               (check-nested))
;;                           nil))
;;                      ((char= (current se) #\]) t) ; End of parent enclosing expression
;;                      (t nil)))
;;              (check-nested-sequential () ; Never called with empty SE
;;                (cond ((emptyp se) nil)
;;                      ((check-nested)
;;                       (cond ((emptyp se) nil)
;;                             ((char= (current se) #\])
;;                              (advance se)
;;                              t)))
;;                      (t nil))))
;;                       ;;              nil))))
;;                       ;; (t nil))))
;;       (and (check-sequential)
;;            (if (emptyp se)
;;                t
;;                (warn se)))) ))

;;;
;;;    Here it is. Finally found the symmetry I was expecting between sequential/nested expressions!!
;;;    
(defun recursive-balanced (s)
  (let ((se (make-instance 'string-enumerator :source s)))
    (labels ((check-sequential ()
               (cond ((emptyp se) t)
                     ((char= (current se) #\[)
                      (advance se)
                      (cond ((emptyp se) nil)
                            ((check-nested)
                             (cond ((emptyp se) nil)
                                   ((char= (current se) #\])
                                    (advance se)
                                    (check-sequential))
                                   (t nil)))
                            (t nil)))
                     (t nil)))
             (check-nested ()
               (cond ((emptyp se) nil) ; Incomplete enclosing expression (Only on recursive calls.)
                     ((char= (current se) #\[)
                      (advance se)
                      (cond ((emptyp se) nil)
                            ((check-nested) ; Further nested expression
                             (cond ((emptyp se) nil)
                                   ((char= (current se) #\])
                                    (advance se)
                                    (check-nested)) ; Sequential nested expression
                                   (t nil)))
                            (t nil)))
                     ((char= (current se) #\]) t) ; End of parent enclosing expression
                     (t nil))))
      (check-sequential))))

;;;
;;;    Earlier version
;;;    
(defun recursive-balanced-stream (s)
  (let ((stream (make-string-input-stream s)))
    (labels ((current ()
               (peek-char nil stream nil nil))
             (advance ()
               (read-char stream nil nil))
             (check-balanced ()
               (cond ((null (current)) t)
                     ((char= (current) #\[)
                      (advance)
                      (case (current)
                        (#\[ (check-start))
                        (#\] (check-end))
                        (otherwise nil)))
                     (t nil)))
             (check-start ()
               (cond ((check-balanced)
                      (cond ((null (current)) nil)
                            ((char= (current) #\]) (check-end))
                            (t nil)))
                     (t nil)))
             (check-end ()
               (advance)
               (case (current)
                 (#\[ (check-balanced))
                 ((nil #\]) t)
                 (otherwise nil))))
      (and (check-balanced)
           (null (listen stream)))) ))

;;;
;;;    Final version!
;;;    
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
      (check-sequential))))

;; (defun check-balanced-iterative (s)
;;   (do ((se (make-instance 'string-enumerator :source s))
;;        (count 0))
;;       ((emptyp se) (zerop count))
;;     (cond ((char= (current se) #\[) (incf count))
;;           ((char= (current se) #\]) (decf count))
;;           (t (return nil)))
;;     (advance se)
;;     (when (minusp count)
;;       (return nil))))
      
(defun check-balanced-iterative (s)
  (do ((se (make-instance 'string-enumerator :source s))
       (count 0))
      ((emptyp se) (zerop count))
    (case (current se)
      (#\[ (incf count))
      (#\] (decf count))
      (otherwise (return nil)))
    (advance se)
    (when (minusp count)
      (return nil))))
      
;; (defun check-balanced-iterative (s)
;;   (do ((i 0 (1+ i))
;;        (count 0))
;;       ((= i (length s)) (zerop count))
;;     (cond ((char= (char s i) #\[) (incf count))
;;           ((char= (char s i) #\]) (decf count))
;;           (t (return nil)))
;;     (when (minusp count)
;;       (return nil))))

(defun check-balanced-iterative (s)
  (do ((i 0 (1+ i))
       (count 0))
      ((= i (length s)) (zerop count))
    (case (char s i)
      (#\[ (incf count))
      (#\] (decf count))
      (otherwise (return nil)))
    (when (minusp count)
      (return nil))))

;; (defun check-balanced-iterative (s)
;;   (loop with count = 0
;;         for ch across s
;;         never (minusp 0) ; This won't catch a final mismatched ]
;;         if (char= ch #\[)
;;           do (incf count)
;;         else if (char= ch #\])
;;           do (decf count)
;;         else
;;           do (return nil)
;;         end
;;         finally (return (zerop count)))) ; But this will.

(defun check-balanced-iterative (s)
  (loop with count = 0
        for ch across s
        never (minusp 0) ; This won't catch a final mismatched ]
        do (case ch
             (#\[ (incf count))
             (#\] (decf count))
             (otherwise (return nil)))
        finally (return (zerop count)))) ; But this will.

(defun check-balanced-stack (s)
  (do ((se (make-instance 'string-enumerator :source s))
       (stack (make-instance 'linked-stack :type 'character)))
      ((emptyp se) (containers:emptyp stack))
    (cond ((char= (current se) #\[) (push stack (current se)))
          ((and (char= (current se) #\]) (not (containers:emptyp stack))) (pop stack))
          (t (return nil)))
    (advance se)))
  
(defun check-balanced-stack (s)
  (do ((se (make-instance 'string-enumerator :source s))
       (stack (make-instance 'linked-stack :type 'character)))
      ((emptyp se) (containers:emptyp stack))
    (case (current se)
      (#\[ (push stack (current se)))
      (#\] (if (containers:emptyp stack)
               (return nil)
               (pop stack)))
      (otherwise (return nil)))
    (advance se)))
  
(defun check-balanced-stack (s)
  (loop with stack = (make-instance 'linked-stack :type 'character)
        for ch across s
        if (char= ch #\[)
          do (push stack ch)
        else if (and (char= ch #\]) (not (containers:emptyp stack)))
          do (pop stack)
        else
          do (return nil)
        end
        finally (return (containers:emptyp stack))))

(defun check-balanced-stack (s)
  (loop with stack = (make-instance 'linked-stack :type 'character)
        for ch across s
        do (case ch
             (#\[ (push stack ch))
             (#\] (if (containers:emptyp stack)
                      (return nil)
                      (pop stack)))
             (otherwise (return nil)))
        finally (return (containers:emptyp stack))))

;(test-balanced #'check-balanced-stack)

(deftest test-balanced (f)
  (check
   (funcall f "")
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
