(defpackage :predicates
  (:use :common-lisp :util)
  (:export :load-predicates)
  (:export :load-predicate)
  )

(in-package :predicates)

(defun load-predicates (path)
	
)

(defun load-predicate (path)
	(load path)
	;(util:read-file-to-string path)
)