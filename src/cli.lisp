(defpackage :cli
  (:use :common-lisp :lexer)
  (:export :run))

(in-package :cli)

(defun draw-menu ()
  (format t "Menu:
   l: lex file
   q: quit
> "))

(defun run (&optional (first t)) 
  (when first
    (draw-menu))
  (block main-loop	 
    (let ((char (read-char)))
      (cond ((equal char #\l)
	     (progn (format t "Enter path: ")
		    (ask-path)))
	    ((equal char #\q)
	     (return-from main-loop))
	    (T (format t "no function for ~a defined" char))))
    (run nil)))

(defun ask-path ()
  (let ((path (read-line)))
    (lex-file path)))
