#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               unicode.lisp
;;;;
;;;;   Started:            Wed Jul 12 02:19:41 2023
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
;;;;   Notes: Fix i = 13!!
;;;;   ~:@(...~) necessary for Allegro!
;;;;

(loop for i from 0 to 1023
      for ch = (code-char i)
      do (format t "~4D ~:@(U+~4,'0X~) ~C ~A~%" i i ch (char-name ch)))
