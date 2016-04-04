(defpackage :predicates
  (:use :common-lisp :util :lexer)
  (:export :load-predicates)
  (:export :load-predicate)
  (:export :setup-predicates)
  (:export :get-predicate)
  (:export :get-predicate-func)
  (:export :is-predefined-predicate)
  (:export :eval-predicate)
  (:export :add-predicate)
 )

(in-package :predicates)

(defvar *predicates*)

(defun load-predicates (path)
	(mapcar 'load-predicate path)
)

(defun load-predicate (path)
	;(print (list "path" path))
	(let ((content (dateilesen path))
		  (pred-name (pathname-name path)))
		
		(add-predicate pred-name (eval (first content)))	
	)
)

(defun setup-predicates ()
	(setq *predicates* (list ))
)

(defun add-predicate(pred-name pred-func)
	 (let ((sym (gensym "PRED-")))
		(import sym)
		(setf (get sym 'name) pred-name)
		(setf (get sym 'func) pred-func)
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
	(if (get-predicate (get lit 'lexer:name)) T NIL)
)

(defun eval-predicate (pred-name args)
	(if (get-predicate pred-name) 
		 (let ((proc-args (mapcar (lambda (arg) (get arg 'lexer:name)) args)))
		 	(apply (get-predicate-func pred-name) proc-args)
		 )
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
	