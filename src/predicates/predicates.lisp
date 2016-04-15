(defpackage :predicates
  (:use :common-lisp :util :lexer :parser)
  (:export :load-predicates)
  (:export :load-predicate)
  (:export :setup-predicates)
  (:export :get-predicate)
  (:export :get-predicate-func)
  (:export :is-predefined-predicate)
  (:export :eval-predicate)
  (:export :add-predicate)
  (:export :get-clause-predicates)
 )

(in-package :predicates)

(defvar *predicates*)

(defun load-predicates (path)
	(mapcar 'load-predicate path)
)

(defun load-predicate (path)
	;(print (list "path" path))
		; demo für predicate in clr sprache
	(if (string-equal "clr" (pathname-type path))
		(let ((parsed (parser:parse-file path))
			  (pred-name (pathname-name path)))
			(if (not parsed) (return-from load-predicates))
			;(print "clr predicate, not implemented yet!")
			(add-predicate pred-name parsed 'CLR)
		)
		(let ((content (dateilesen path))
			  (pred-name (pathname-name path)))
			(add-predicate pred-name (eval (first content)) 'LISP)	
		)
	)
)

(defun get-clause-predicates ()
	(let ((clauses (mapcar (lambda (x) (get (cdr x) 'func)) (remove-if-not (lambda (x) (equal (get (cdr x) 'type) 'CLR)) *predicates*))))
		(apply 'append clauses)
	)
)


(defun setup-predicates ()
	(setq *predicates* (list ))
)

(defun add-predicate(pred-name pred-func pred-type)
	 (let ((sym (gensym "PRED-")))
		(import sym)
		(setf (get sym 'name) pred-name)
		(setf (get sym 'func) pred-func)
		(setf (get sym 'type) pred-type)
		(setq *predicates* (acons pred-name sym *predicates*))
	 )
)

(defun get-predicate (pred-name)
	(cdr (assoc pred-name *predicates* :test #'string=))
)

(defun get-predicate-func (pred-name)
	(get (get-predicate pred-name) 'func)
)

(defun is-predefined-predicate (lit)
	(let ((predicate (get-predicate (get lit 'lexer:name))))
		(cond ((not predicate) NIL)
			((equal (get predicate 'type) 'LISP) T)
			(T NIL)
		)
	)
)

(defun eval-predicate (pred-name args)
	(if (get-predicate pred-name) 
		; (let ((proc-args (mapcar (lambda (arg) (get arg 'lexer:name)) args)))
		 	(apply (get-predicate-func pred-name) args)
		; )
		NIL
	)
)

;;Move to utils
(defun dateilesen (dateiname)
  (do* ((streamin (open dateiname))
        exprs
        (expr (read streamin nil 'eof)
              (read streamin nil 'eof)))
       ((equal expr 'eof) (close streamin)
        (nreverse exprs))
    (setq exprs (cons expr exprs)))) 
	