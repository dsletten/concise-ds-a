;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               defended.lisp
;;;;
;;;;   Started:            Tue May 17 11:14:02 2022
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
;;;;   Notes: Make slots invisible! Only accessible via functions defined before slot names
;;;;   are uninterned. This breaks existing instances when the package is reloaded!!!
;;;;
;;;;

(defpackage :defended (:use :common-lisp)

(in-package :defended)

(defclass foo ()
  ((slot1 :reader height :initarg :height)
   (slot2 :accessor age :initarg :age)))

(defun bar (foo)
  (with-slots (slot1 slot2) foo
    (list slot1 slot2)))

(unintern 'slot1)
(unintern 'slot2)

;; (defvar *def* (make-instance 'defended::foo :height 20 :age 7))

;; (describe *def*)
;; #<DEFENDED::FOO {1002F87B73}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1                          = 20
;;   SLOT2                          = 7

;; (setf (defended::age *def*) 99)
;; 99

;; (describe *def*)
;; #<DEFENDED::FOO {10042B3F33}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   SLOT1                          = #<unbound slot>
;;   SLOT2                          = 99

;; (slot-value *def* 'defended::slot2)

;; debugger invoked on a SB-PCL::MISSING-SLOT in thread
;; #<THREAD "main thread" RUNNING {1001B701F3}>:
;;   When attempting to read the slot's value (slot-value), the slot
;;   DEFENDED::SLOT2 is missing from the object #<DEFENDED::FOO {10042B3F33}>.
;; It has a slot #:SLOT2, while DEFENDED::SLOT2 is requested.

;; Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

;; restarts (invokable by number or by possibly-abbreviated name):
;;   0: [ABORT] Exit debugger, returning to top level.

;; ((:METHOD SLOT-MISSING (T T T T)) #<STANDARD-CLASS DEFENDED::FOO> #<DEFENDED::FOO {10042B3F33}> DEFENDED::SLOT2 SLOT-VALUE NIL) [fast-method]
;; 0] :a

;; (defended::age *def*)
;; 99

;; (defended::bar *def*)
;; (20 99)
