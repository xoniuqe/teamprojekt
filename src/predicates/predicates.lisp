(defpackage :predicates
  (:use :common-lisp :util)
  (:export :load-predicates)
  (:export :load-predicate)
  )

(in-package :predicates)

(defun load-predicates (path)
	
)

(defun load-predicate (path)
	;(load path)
	(let ((lisp-printed-string
       (with-output-to-string (*standard-output*)
         (eval (read path t t t)))))
  ;; concatenate the lisp printed string onto your 
  ;; hand parsed string here
  )
)