 (eval-when (:compile-toplevel :load-toplevel :execute) (asdf:operate 'asdf:load-op :cl-ppcre))

(defpackage :lexer
  (:use :common-lisp :cl-ppcre :util)
  (:export :lexer :lex-file :make-term-lexer :path
	   :forall :exists :and :in :end :<= :=> :var :lit
	   :LB :RB :LS :CONS :LE :SEP :INT :SYM :ARGS :TYPE :NAME :LITERAL))

(in-package :lexer)

;(setq *regex-prog* "forall|exists|and|<=|=>|:|[a-zA-Z0-9]+\\(.*\\)(?=\\s*<=)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*and)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*=>)|[a-zA-Z0-9]+\\(.*\\)(?=\\.)|[a-z]+|\\.")
;(setq *regex-prog* "forall|exists|and|<=|=>|:|[a-zA-Z0-9]+\\(.*\\)(?=\\s*<=)|[a-zA-Z0-9]+\\([^)]*\\)(?=\\s*and)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*=>)|[a-zA-Z0-9]+\\(.*\\)(?=\\.)|[a-z]+|\\.")
;;; Changed and added by Tobias Arens
(setq *regex-prog* "forall|exists|and|<=|=>|:|[a-zA-Z0-9]+\\(.*\\)(?=\\s*<=)|[a-zA-Z0-9]+\\([^()]*\\)(?=\\s*and)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*=>)|[a-zA-Z0-9]+\\(.*\\)(?=\\.)|[a-z]+|\\.")
(setq *regex-nested* "and|[a-zA-Z0-9]+\\(.*\\)(?=\\s*and)|[a-zA-Z0-9]+\\(.*\\)")
;;; Changes end
(defvar *regex-term* "\\d+|\\w+|.")

;;; test programm
(setq str "forall x y: p(f(h(x),x,h(x))) and p(f(z,h(w),h(z))) => q(x).")
(setq clause (car (cl-ppcre:all-matches-as-strings ".+?\\." str)))
(setq l (cl-ppcre:all-matches-as-strings *regex-prog* clause))

;; (setq path "/home/steven/dev/lisp/cl-reason/test/test_prog.clr")
;(defvar path "/home/steven/dev/lisp/cl-reason/test/test_prog.clr")

(defun split-into-tokens (str)
	(print "split-into-tokens")
  (mapcan (lambda (clause)
		;(print clause)
		(print  (cl-ppcre:all-matches-as-strings *regex-prog* clause))
	    (cl-ppcre:all-matches-as-strings *regex-prog* clause))
	  (cl-ppcre:all-matches-as-strings ".+?\\." str)))

(defun lex-list (l)
  "replaces strings in 'l' with appropriate tokens"
  (mapcar (lambda (x)
  		(print (list "lex-list: " x))
	    (setf tmp (cond ((string= x "forall") (make-token "lexer::forall"))
		  ((string= x "exists") (make-token "lexer::exists"))
		  ((string= x "and") (make-token "lexer::and"))
		  ((string= x "=>") (make-token "lexer::=>"))
		  ((string= x "<=") (make-token "lexer::<="))
		  ((string= x ".") (make-token "lexer::end"))
		  ((string= x ":") (make-token "lexer::in"))
		  ((= (length x) 1) (make-variable-sym x))
		  (t (make-literal-sym x))))
		 ;(print tmp)
		 tmp)
	  l))

(defun make-token (s)
  (let ((sym (gensym (concatenate 'string s "-"))))
    (import sym)
    (setf (symbol-value sym) (read-from-string s))
    sym))

(defun make-variable-sym (x)
  (let ((sym (gensym "var-")))
    (import sym)
    (setf (get sym 'name) x)
    (setf (get sym 'type) 'variable)
    (setf (symbol-value sym) 'var)
    sym))

(defun make-literal-sym (x)
  (let ((sym (gensym "lit-")))
    (import sym)
    (setf (get sym 'name) (cl-ppcre:scan-to-strings "\\w+" x))
    (setf (get sym 'args) (tokenize-terms (cl-ppcre:scan-to-strings "\\(.*\\)" x)))
    (setf (get sym 'type) 'literal)
    (setf (symbol-value sym) 'lit)
    sym))

	
;;; Changed by Tobias Arens
;;; added a extra regex check for wrong lexed terms that could occurr if there were nested brackets
;;; Simply loop other every element and run an regex and check if something changed
;;; e.g "predicate(nested(bracket),x) and predicate(nested(bracket),y)" is read as one token but should be three
(defun lexer (text)
	(let* ((splitted (split-into-tokens text))
		 (nestedregex (remove nil(mapcar 
					 (lambda (x)
						(let ((result (cl-ppcre:all-matches-as-strings *regex-nested* x)))
							(if (or (not result) (eq (length result) 1)) NIL (list x result))
						))		 
					splitted))))
	
	;(setf testtest (remove nil testtest))
	(setf replacedlist nil)
	(setf listcopy splitted)
	(do* ((current (car nestedregex))
	       
		   )
		 ((not nestedregex))
		(let* ((pos (position (first current) listcopy :test 'string-equal))
			  (listto(subseq listcopy 0 pos )))
		(setf listcopy (subseq listcopy (+ pos 1)))
		(setf replacedlist (append replacedlist listto (second current)))
		)
		(setf nestedregex (cdr nestedregex))
	)
	;; Append the rest list
	(setf replacedlist (append replacedlist listcopy))
	(lex-list replacedlist)
  ))

(defun lex-file (path)
  (lexer (read-file-to-string path)))


;;; (setq termstring "(f(z,h(w),h(z)))")
(defun tokenize-terms (termstring)
  (mapcar (lambda (x)
	    (cond ((string= x "(") 'LB)
		  ((string= x ")") 'RB)
		  ((string= x "[") 'LS)
		  ((string= x "|") 'CONS)
		  ((string= x "]") 'LE)
		  ((string= x "+") '+)
		  ((string= x "-") '-)
		  ((string= x "*") '*)
		  ((string= x "/") '/)
		  ((string= x ",") 'SEP)
		  ((cl-ppcre:scan "\\d+" x) (parse-integer x))
		  (T (read-from-string x))))
	  (remove " " (cl-ppcre:all-matches-as-strings *regex-term* termstring) :test 'string=)))

(defun make-term-lexer (symbol-list)
  (lambda ()
    (let ((value (pop symbol-list)))
      (if (null value)
	  (values nil nil)
	(let ((terminal (cond ((member value '(LB RB LS CONS LE + - * / SEP)) value)
			      ((integerp value) 'INT)
			      ((symbolp value) 'SYM)
			      (T (error "Unexpected value ~S" value)))))
	  (values terminal value))))))
