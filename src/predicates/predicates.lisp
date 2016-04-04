(defpackage :predicates
  (:use :common-lisp :util :lexer)
  (:export :load-predicates)
  (:export :load-predicate)
  (:export :setup-predicates)
  (:export :get-predicate)
  (:export :get-predicate-func)
  (:export :is-predefined-predicate2)
 )

(in-package :predicates)

(defvar *predicates*)

(defun load-predicates (path)
	
)

(defun load-predicate (path)
	(dateilesen path)
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
	
(defun predicates-test () 
	(let ((sym (gensym "PRED-")))
		(import sym)
		(setf (get sym 'name) "issteven")
		(setf (get sym 'func) (lambda (x) (string= x "steven")))
		sym
	)
)


(defun setup-predicates ()
	(setq *predicates* (list ))
	(add-predicate "iststeven" (lambda (v) (string-equal v "steven")))
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

(defun is-predefined-predicate2 (lit)
	(if (get-predicate (get lit 'lexer:name)) T NIL)
)

(defun eval-predicate (pred-name args)
	(if (get-predicate pred-name) 
		(apply (get-predicate-func pred-name) args)
		NIL
	)
)