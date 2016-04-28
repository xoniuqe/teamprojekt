(defpackage :helper
  (:use :common-lisp :lexer :util :parser :resolution)
  (:export :calc-result :check-fun-args-const :eval-fun))

(in-package :helper)

(defun calc-result (result)
	(let ((type (get result 'resolution::type)))
		(cond ((equal type 'variable) (get result 'resolution::name))
					  ((and (equal type 'function) (string-equal (get result 'lexer::name) "cons")) (cons-to-clr-list (eval-fun result)))
                      ((equal type 'function) (eval-fun result))
                      ((equal type 'parser::const) (get result 'resolution::name))
			  )
	)
)


(defun check-fun-args-const (fun)
	(let* ((operator (get fun 'lexer:name))
		(result (mapcar (lambda (argument) 
				(let ((type (get argument 'type)))
					(cond ((equal type 'function) (check-fun-args-const argument))
						  ((equal type 'parser::const) T)
						  (T return-from check-fun-args-const NIL)
					)
				)
			)
		(get fun 'lexer:args))))
		(mapcar (lambda (x) (if (not x) (return-from check-fun-args-const NIL))) result)
		(return-from check-fun-args-const T)
	)
)

(defun eval-fun (fun)
	(let ((operator (get fun 'lexer::name))
		(value-list (mapcar (lambda (argument) 
				(let ((type (get argument 'type))
					  (nvalue (get argument 'parser::value)))
					(cond ((not nvalue) (setq nvalue (get argument 'parser::name)))
						 (T NIL))
					(cond ((equal type 'function) (eval-fun argument))
						  ((equal type 'parser::const) (list nvalue))
						  (T NIL)
					)
				)
			)

		(get fun 'lexer:args))))
		(cond ((string-equal operator "+") (apply '+ value-list))
			  ((string-equal operator "-") (apply '- value-list))
			  ((string-equal operator "*") (apply '* value-list))
			  ((string-equal operator "/") (apply '/ value-list))
              ((string-equal operator "cons") (apply 'append value-list))
			(T (list operator value-list)))
	)
)

(defun cons-to-clr-list (cons-list)
	(print cons-list)
	(setf result '([))
	(mapcar (lambda (elem) 
		 (setf result (append result (list elem)))) cons-list)
	(append result '(]))
)
