;;       _                                             
;;  ___ | |        _ __  ___   __ _  ___   ___   _ __  
;; / __|| | _____ | '__|/ _ \ / _` |/ __| / _ \ | '_ \ 
;;| (__ | ||_____|| |  |  __/| (_| |\__ \| (_) || | | |
;; \___||_|       |_|   \___| \__,_||___/ \___/ |_| |_|
;;
;; Autor: Steven Kutsch (kutsch.steven@gmail.com)

#-asdf (load  "/lib/asdf")
(pushnew "/registry/" asdf:*central-registry* :test #'equal)

(asdf:operate 'asdf:load-op :cl-ppcre)
(asdf:operate 'asdf:load-op :yacc)

(defpackage :cl-reason
  (:use :common-lisp :cl-ppcre :yacc)
  (:export :run-program))

(in-package :cl-reason)

;; -- Util ------------------------------------------------------------------------------
;; Hilfsfunktionen

(defun read-file-to-string (path)
  (with-open-file (stream path) ; TODO: fehler falls datei nicht existiert
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun take (n l)
  (cond ((< (length l) n) l)
	(t (subseq l 0 n))))

(defun period-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\. string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun intersperse (obj ls)
  (cons (car ls) (intersperse-rec obj (reverse (cdr ls)) nil)))

(defun intersperse-rec (obj ls acc)
  (if ls
      (intersperse-rec obj (cdr ls) (cons obj (cons (car ls) acc)))
     acc))

;; ToDo:
;;   - Behandlung von Listen
;;   - Behandlung von arithmetischen Ausdrücken
;;   - Behandlung von Zahlen
(defun term-to-string (term)
  (let ((termtype (get term 'type)))
    (cond ((or (equal termtype 'variable)
	       (equal termtype 'const)) (get term 'name))
	  (T (concatenate 'string (get term 'name)
			  "("
			  (eval (append '(concatenate 'string)
					(intersperse "," (mapcar 'term-to-string
								 (get term 'args)))))
			  ")")))))

(defun print-term (term)
  (print (term-to-string term)))

;; -- Lexer -------------------------------------------------------------------------
;; lexikalische Analyse

(setq *regex-prog* "forall|exists|and|<=|=>|:|[a-zA-Z0-9]+\\(.*\\)(?=\\s*<=)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*and)|[a-zA-Z0-9]+\\(.*\\)(?=\\s*=>)|[a-zA-Z0-9]+\\(.*\\)(?=\\.)|[a-z]+|\\.")

(defvar *regex-term* "\\d+|\\w+|.")

;;; test programm
(setq str "forall x y: p(f(h(x),x,h(x))) and p(f(z,h(w),h(z))) => q(x).")
(setq clause (car (cl-ppcre:all-matches-as-strings ".+?\\." str)))
(setq l (cl-ppcre:all-matches-as-strings *regex-prog* clause))

;; (setq path "/home/steven/dev/lisp/cl-reason/test/test_prog.clr")
(defvar path "/home/steven/dev/lisp/cl-reason/test/test_prog.clr")

(defun split-into-tokens (str)
  (mapcan (lambda (clause)
	    (cl-ppcre:all-matches-as-strings *regex-prog* clause))
	  (cl-ppcre:all-matches-as-strings ".+?\\." str)))

(defun lex-list (l)
  "replaces strings in 'l' with appropriate tokens"
  (mapcar (lambda (x)
	    (cond ((string= x "forall") (make-token "cl-reason::forall"))
		  ((string= x "exists") (make-token "cl-reason::exists"))
		  ((string= x "and") (make-token "cl-reason::and"))
		  ((string= x "=>") (make-token "cl-reason::=>"))
		  ((string= x "<=") (make-token "cl-reason::<="))
		  ((string= x ".") (make-token "cl-reason::end"))
		  ((string= x ":") (make-token "cl-reason::in"))
		  ((= (length x) 1) (make-variable-sym x))
		  (t (make-literal-sym x))))
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

(defun lexer (text)
  (lex-list (split-into-tokens text)))

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

;; -- Parser ----------------------------------------------------------------------------------

(defun parse-file (path)
  (mapcar 'reverse-neg-lits (interpret-symbols-in-clauses (parse-program (lex-file path)))))

(defun parse-program (input)
  (do* ((ntl '(s))
	(program input)
	(output NIL)
	(la (take 2 program) (take 2 program))
	(expansion (funcall (next-nt ntl) la) (funcall (next-nt ntl) la))
	(production (nth 2 expansion) (nth 2 expansion))
	(to-match (car expansion) (car expansion))
	(new-nt (cadr expansion) (cadr expansion)))
       ((and (not program) (not ntl)) (reverse output))
    (case production
      (0 (setq output (cons (new-clause) output)))
      (1 (setq output (cons (new-clause 'goal) output)))
      (2 (progn (setq output (cons (new-clause) output))
		(setf (get (car output) 'pos-lit) (parse-terms-in-lit (car program)))))
      (3 nil)
      (4 (setf (get (car output) 'vars) (cons (car program) (get (car output) 'vars))))
      (5 (setf (get (car output) 'vars) (reverse (cons (car program)
						       (get (car output) 'vars)))))
      (6 (setf (get (car output) 'pos-lit) (parse-terms-in-lit (car program))))
      (7 (setf (get (car output) 'neg-lits) (cons (parse-terms-in-lit (car program))
						  (get (car output) 'neg-lits))))
      (8 (setf (get (car output) 'pos-lit) (parse-terms-in-lit (car program))))
      (9 nil)
      (11 (progn (setf (get (car output) 'neg-lits)
		       (cons (parse-terms-in-lit (car program))
			     (get (car output) 'neg-lits)))
		 (setf (get (car output) 'pos-lit) (parse-terms-in-lit (caddr program))))))
    (setq program (match (car expansion) program))
    (setq ntl (append (cadr expansion) (cdr ntl)))))

(defun match (x input)
  (let ((match_length (length x)))
    (cond ((la-check (take match_length input) x)
	   (subseq input match_length))
	  (T (error "match error")))))

(defun s (la)
  (cond ((la-check la '(forall var))
	 '((forall) (v k s) 0))
	((la-check la '(exists var))
	 '((exists) (v k s) 1))
	((la-check la '(lit end))
	 '((lit end) (s) 2))
	((la-check la NIL)
	 '(nil nil 3))
	(T (error "no fitting production"))))

(defun v (la)
  (cond ((la-check la '(var var))
	 '((var) (v) 4))
	((la-check la '(var in))
	 '((var in) nil 5))
	(T (error "no fitting production"))))

(defun k (la)
  (cond ((la-check la '(lit <=))
	 '((lit <=) (f) 6))
	((la-check la '(lit and))
	 '((lit and) (f2) 7))
	((la-check la '(lit end))
	 '((lit end) nil 8))
	((la-check la '(lit =>))
	 '(() (f) 9))
	(T (error "no fitting production"))))

(defun f (la)
  (cond ((la-check la '(lit end))
	 '((lit end) nil 7))
	((la-check la '(lit and))
	 '((lit and) (f) 7))
	(T (error "no fitting production"))))

(defun f2 (la)
  (cond ((la-check la '(lit end))
	 '((lit end) nil 7))
	((la-check la '(lit =>))
	 '((lit => lit end) nil 11))
	((la-check la '(lit and))
	 '((lit and) (f) 7))
	(T (error "no fitting production"))))

(defun next-nt (l)
  (cond ((not l) (lambda (x) x))
	(t (car l))))

(defun la-check (la match)
  (let ((new-la (mapcar 'symbol-value la)))
    (equal new-la match)))

(defun new-clause (&optional (clause-type 'program))
  (let ((sym (gensym "clause-")))
    (import sym)
    (case clause-type
      (program (setf (get sym 'type) 'prog-clause))
      (goal (setf (get sym 'type) 'goal-clause)))
    sym))

(defun reverse-neg-lits (clause)
  (setf (get clause 'neg-lits)
	(reverse (get clause 'neg-lits)))
  clause)

;;;--- Term parser -----------------------------------------------------------
;;; test-terme: P(x,3 + 123,[f|r],y)
(defvar test-terme '(LB AND LB NOT LB X RB SEP Y RB SEP ERG RB))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))
  
  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b)

  (defun ignore-sec (a b c)
    (declare (ignore b))
    (list a c))

  (defun define-arith (a1 op a2)
    (let ((sym (gensym "ae-")))
      (import sym)
      (setf (get sym 'type) 'arith-exp)
      (setf (get sym 'args) (list a1 a2))
      (setf (get sym 'name) (write-to-string op))
      (setf (get sym 'exp) (list op a1 a2))
      sym))

  (defun define-concrete-list (a elems b)
    (declare (ignore a b))
    (let ((sym (gensym "list-")))
      (import sym)
      (setf (get sym 'name) "cons")
      (setf (get sym 'type) 'concrete-list)
      (setf (get sym 'elems) elems)
      sym))

  (defun define-abstract-list (a elem b rest c)
    (let ((sym (gensym "list-")))
      (import sym)
      (setf (symbol-value sym) (list a elem b rest c))
      (setf (get sym 'name) "cons")
      (setf (get sym 'type) 'abstract-list)
      (setf (get sym 'first) elem)
      (setf (get sym 'rest) rest)
      (setf (get sym 'args) (list elem rest))
      ;;(setf (get sym 'elems) (list elem rest))
      sym))

  (defun define-sym-fun (name x args y)
    (declare (ignore x y))
    (let ((sym (gensym "fun-")))
      (import sym)
      (setf (get sym 'type) 'function)
      (setf (get sym 'name) (string-downcase (write-to-string name)))
      (setf (get sym 'args) (condense-terms args))
      sym)))

(define-parser *term-parser*
  (:start-symbol S)
  (:terminals (LB RB LS CONS LE + - * / SEP INT SYM))
  (:precedence ((:left * /) (:left + -)))
  (s (LB t RB #'k-2-3))
  (t SYM
     a
     l
     f
     (t SEP t #'ignore-sec)
     ())
  (a (a + a #'define-arith)
     (a - a #'define-arith)
     (a * a #'define-arith)
     (a / a #'define-arith)
     (LB a RB #'k-2-3)
     SYM
     INT)
  (f (SYM LB t RB #'define-sym-fun))
  (l (LS t LE #'define-concrete-list)
     (LS SYM CONS SYM LE #'define-abstract-list)
     (LS SYM CONS l LE #'define-abstract-list)))

(defun condense-terms (term-list)
  (cond ((atom term-list) (list term-list))
	((notany 'listp term-list) term-list)
	(T (cons (car term-list) (condense-terms (cadr term-list))))))

(defun parse-terms-in-lit (lit)
  (let ((term-list (get lit 'args)))
    (setf (get lit 'args) (condense-terms
			   (parse-with-lexer (make-term-lexer term-list)
					     *term-parser*)))
    lit))

;;; ---- Interpreter ---------------------------------------------------------

(defun var-member (sym vars)
  (member sym vars :test (lambda (x y) (equal x (read-from-string (get y 'name))))))

;; (setq test-clause (nth 0 (parse-file path)))
;; (setq test-lit (get test-clause 'pos-lit))
;; (setq test-lit2 (cadr (get test-clause 'neg-lits)))
;; (setq test-list (car (get test-lit 'args)))
;; (setq vars (get (car (parse-file path)) 'vars))
(defun interpret-symbols-in-clauses (clauses)
  (mapc (lambda (clause)
	  (let ((vars (get clause 'vars))
		(pos-lit (get clause 'pos-lit))
		(neg-lits (get clause 'neg-lits)))
	    (when pos-lit (interpret-symbols-in-lit pos-lit vars))
	    (when neg-lits (mapc (lambda (x)
				   (interpret-symbols-in-lit x vars))
				 neg-lits))))
	clauses))

;; (setq clause (car (parse-program (lex-file path))))
;; (setq vars (get clause 'vars))
;; (setq lit (car (get clause 'neg-lits)))
;; (setq x (car (get lit 'args)))
(defun interpret-symbols-in-lit (lit vars)
  (setf (get lit 'args)
	(mapcar (lambda (x)
		  (cond ((and (atom x)
			      (var-member x vars))
			 (car (var-member x vars)))
			((numberp x) (let ((sym (gensym "const-")))
				       (import sym)
				       (setf (get sym 'type) 'const)
				       (setf (get sym 'name)
					     (concatenate 'string
							  "number-"
							  (write-to-string x)))
				       (setf (get sym 'value) x)
				       sym))
			((equal (get x 'type) 'function)
			 (interpret-symbols-in-lit x vars))
			((or (equal (get x 'type) 'concrete-list)
			     (equal (get x 'type) 'abstract-list))
			 (interpret-symbols-in-lit (interpret-list x) vars))
			((equal (get x 'type) 'arith-exp)
			 (interpret-symbols-in-lit x vars))
			((equal (get x 'type) 'const) x)
			((atom x) (let ((sym (gensym "const-")))
				    (import sym)
				    (setf (get sym 'type) 'const)
				    (setf (get sym 'name)
					  (string-downcase (write-to-string x)))
				    sym))
			(T x)))
		(if (listp (get lit 'args))
		    (get lit 'args)
		  (list (get lit 'args)))))
  lit)

(defun interpret-list (list-sym)
  (let ((listtype (get list-sym 'type))
	(elems (get list-sym 'elems)))
    (cond ((equal listtype 'abstract-list)
	   (progn (setf (get list-sym 'type) 'function)
		  list-sym))
	  ((not elems) (empty-list))
	  ((equal listtype 'concrete-list)
	   (progn
	     (setf (get list-sym 'args) (translate-elems elems))
	     (setf (get list-sym 'type) 'function)
	     list-sym)))))

(defun def-list (elems)
  (let ((sym (gensym "list-")))
    (import sym)
    (setf (get sym 'type) 'function)
    (setf (get sym 'name) "cons")
    (setf (get sym 'args) elems)
    sym))

(defun empty-list ()
  (let ((sym (gensym "const-")))
    (import sym)
    (setf (get sym 'type) 'const)
    (setf (get sym 'name) "empty")
    sym))

(defun translate-elems (elems)
  (cond ((atom elems) (list elems (empty-list)))
	((= 1 (length elems)) (list (car elems) (empty-list)))
	(T (list (car elems) (def-list (translate-elems (cadr elems)))))))

;; -- Logik -----------------------------------------------------------------------------

;;--- Unification ----------------------------------------------------------------

;; depracated
(defun convert-term-symbol (termsym)
  (let ((termtype (get termsym 'type)))
    (cond ((equal termtype 'variable) (concatenate 'string "var-" (get termsym 'name)))
	  ((equal termtype 'const) (concatenate 'string "con-" (get termsym 'name)))
	  (T (cons (get termsym 'name)
		   (mapcar 'convert-term-symbol (get termsym 'args)))))))

(defun copy-term-symbol (termsym)
  (let ((termtype (get termsym 'type)))
    (cond ((equal termtype 'variable) termsym)
	  ((equal termtype 'const) termsym)
	  (T (let ((sym (gensym "fun-")))
	       (import sym)
	       (setf (get sym 'type) 'function)
	       (setf (get sym 'name) (get termsym 'name))
	       (setf (get sym 'args) (mapcar 'copy-term-symbol (get termsym 'args)))
	       sym)))))

(defun copy-literal (literal)
  (let ((sym (gensym "lit-")))
    (import sym)
    (setf (get sym 'type) 'literal)
    (setf (get sym 'args) (mapcar 'copy-term-symbol (get literal 'args)))
    (setf (get sym 'name) (get literal 'name))
    sym))

(defun copy-clause (clause)
  (let ((sym (gensym "clause-"))
	(pos-literal (get clause 'pos-lit))
	(neg-literals (get clause 'neg-lits)))
    (import sym)
    (setf (get sym 'clause-type) (get clause 'clause-type))
    (setf (get sym 'pos-lit) (copy-literal pos-literal))
    (setf (get sym 'neg-lits) (mapcar 'copy-literal neg-literals))
    sym))

(defun term-is-var (term)
  (equal (get term 'type) 'variable))

(defun term-is-const (term)
  (equal (get term 'type) 'const))

(defun term-get-name (term)
  (get term 'name))

(defun term-is-fun (term)
  (equal (get term 'type) 'function))

(defun term-is-arith (term)
  (equal (get term 'type) 'arith-exp))

;; (setq term1 (car t1))
;; (setq term2 (car t2))
(defun term-equal (term1 term2)
  (let ((termtype1 (get term1 'type))
	(termtype2 (get term2 'type)))
    (cond ((and (equal termtype1 'variable)
		(equal termtype2 'variable))
	   (equal term1 term2))
	  ((and (equal termtype1 'const)
		(equal termtype2 'const))
	   (string= (get term1 'name) (get term2 'name)))
	  ((and (equal termtype1 'function)
		(equal termtype2 'function))
	   (and (string= (get term1 'name)
			 (get term2 'name))
		(= (length (get term1 'args))
		   (length (get term2 'args)))
		(every 'term-equal (get term1 'args) (get term2 'args)))))))

;; (setq lit1 cur-subgoal)
;; (setq lit2 next-lit)
(defun unify-literals (lit1 lit2) 
  (let ((name1 (get lit1 'name))
	(name2 (get lit2 'name))
	(args1 (mapcar 'copy-term-symbol (get lit1 'args)))
	;; (setq t1 (mapcar 'copy-term-symbol (get lit1 'args)))
	(args2 (mapcar 'copy-term-symbol (get lit2 'args))))
        ;; (setq t2 (mapcar 'copy-term-symbol (get lit2 'args)))
    (if (and (string= name1 name2)
	     (= (length args1)
		(length args2))) 
	(unify-termlists args1 args2 '())
      'fail)))

;; (setq t1 (mapcar 'copy-term-symbol (get test-lit1 'args)))
;; (setq t1 (apply-unifier-to-list u (get (apply-unifier u (car t1)) 'args)))
;; (setq t1 (cdr t1))
;; (setq t2 (mapcar 'copy-term-symbol (get test-lit2 'args)))
;; (setq t2 (apply-unifier-to-list u (get (apply-unifier u (car t2)) 'args)))
;; (setq t2 (cdr t2))
;; (setq u nil)

(defun unify-termlists (t1 t2 u)
  (cond ((and (not t1) (not t2)) u)
	((term-equal (apply-unifier u (car t1))
		     (apply-unifier u (car t2))) (unify-termlists (cdr t1) (cdr t2) u))
	((and (or (term-is-var (apply-unifier u (car t1)))
		  (term-is-var (apply-unifier u (car t2))))
	      (not (var-occur (car (return-var (apply-unifier u (car t1))
					       (apply-unifier u (car t2))))
			      (cadr (return-var (apply-unifier u (car t1))
						(apply-unifier u (car t2)))))))
	 (unify-termlists (cdr t1) (cdr t2)
			  (compose-unifiers
			   u (list
			      (list
			       (car (return-var (apply-unifier u (car t1))
						(apply-unifier u (car t2))))
			       (cadr (return-var (apply-unifier u (car t1))
						 (apply-unifier u (car t2)))))))))
	((and (term-is-const (apply-unifier u (car t1)))
	      (term-is-const (apply-unifier u (car t2)))) 'fail)
	((and (or (and (term-is-fun (apply-unifier u (car t1)))
		       (term-is-fun (apply-unifier u (car t2))))
		  (and (term-is-arith (apply-unifier u (car t1)))
		       (term-is-arith (apply-unifier u (car t2)))))
	      (string= (get (apply-unifier u (car t1)) 'name)
		       (get (apply-unifier u (car t2)) 'name))
	      (= (length (get (apply-unifier u (car t1)) 'args))
		 (length (get (apply-unifier u (car t2)) 'args))))
	 (let ((ru (unify-termlists
		    (apply-unifier-to-list u (get (apply-unifier u (car t1)) 'args))
		    (apply-unifier-to-list u (get (apply-unifier u (car t2)) 'args)) nil)))
	   (if (equal ru 'fail)
	       'fail
	     (unify-termlists (apply-unifier-to-list u (cdr t1))
			      (apply-unifier-to-list u (cdr t2))
			      (compose-unifiers u ru)))))
	(T 'fail)))

;; ToDo:
;; Werden zwei Variablen unifiziert, wird jeweils die Variable aus der Goal-Klausel
;; mir der Variable aus der Programm-Klausel instanziert.
;; So sollte nach erfolgreicher SLD-Resolution der Wert der Goal-Variablen
;; berechnet werden können.

;; Update: sollte irelevant sein, da mit antwort-prädikat gearbeitet wird
(defun return-var (t1 t2)
  (cond ((term-is-var t1) (list t1 t2))
	(T (list t2 t1))))

(defun apply-unifier (u term)
  (cond ((not u) term)
	(T (apply-unifier (cdr u) (term-substitut term (caar u) (cadar u))))))

(defun apply-unifier-to-list (u termlist)
  (mapcar (lambda (x)
	    (apply-unifier u x))
	  termlist))

(defun apply-unifier-to-literal (u literal)
  (setf (get literal 'args)
	(apply-unifier-to-list u (get literal 'args)))
  literal)

(defun compose-unifiers (u1 u2)
  (append (mapcar (lambda (x)
		    (list (car x) (apply-unifier u2 (cadr x))))
		  u1)
	  u2))

;; ersetzt jedes vorkommen der Variable x im Term l durch den Term y
(defun term-substitut (l x y)
  (let ((termtype (get l 'type)))
    (cond ((and (equal termtype 'variable)
		(equal l x)) y)
	  ((equal termtype 'function)
	   (progn
	     (setf (get l 'args)
		   (mapcar (lambda (z)
			     (term-substitut z x y))
			   (get l 'args)))
	     l))
	  (T l))))

(defun deep-member (expr list)
  (cond ((not list) NIL)
	((equal expr (car list)) T)
	((listp (car list)) (or (deep-member expr (car list)) 
				(deep-member expr (cdr list))))
	(T (deep-member expr (cdr list)))))

(defun var-occur (var term)
  (let ((termtype (get term 'type)))
    (cond ((equal termtype 'variable) (equal var term))
	  ((equal termtype 'const) nil)
	  (T (some (lambda (x)
		     (var-occur var x)) (get term 'args))))))

;; liste aller Variablen in einem Term
(defun vars-in-term (term)
  (let ((termtype (get term 'type)))
    (cond ((equal termtype 'variable) (list term))
	  ((equal termtype 'const) '())
	  (T (mapcan (lambda (x)
		       (vars-in-term x))
		     (get term 'args))))))

(defun apply-unifier-to-clause (unifier clause)
  (let* ((pos-literal (get clause 'pos-lit))
	 (neg-lits (get clause 'neg-lits)))
    (setf (get clause 'pos-lit) (apply-unifier-to-literal unifier pos-literal))
    (setf (get clause 'neg-lits) (mapcar (lambda (x)
					   (apply-unifier-to-literal unifier x))
					 neg-lits))
    clause))

(defun print-unifier (u)
  (mapcar (lambda (x)
	    (mapcar (lambda (y)
		      (term-to-string y))
		    x))
	  u))

;; --- Resolution -----------------------------------------------------------

(defun clause-is-goal (clause)
  (equal (get clause 'type) 'goal-clause))

(defun clause-is-prog (clause)
  (equal (get clause 'type) 'prog-clause))

;; returns multiple-values:
;; list of program-clauses
;; list of goals
(defun split-prog-from-goals (program)
  (values (remove-if 'clause-is-goal program)
	  (remove-if 'clause-is-prog program)))

;; negates the clause by setting the pos-lit as a neg-lit
;; and adding the answer-lit as new pos-lit
(defun prepare-goal (clause)
  (let ((answer (gensym "answer-"))
	(sym (gensym "clause-")))
    (import answer)
    (import sym)
    (setf (get answer 'type) 'literal)
    (setf (get answer 'args) (get clause 'vars))
    (setf (get answer 'name) "answer")
    (setf (get sym 'neg-lits) (list (get clause 'pos-lit)))
    (setf (get sym 'pos-lit) answer)
    (setf (get sym 'vars) (get clause 'vars))
    sym))

(defun extract-procedure (program pname)
  (remove-if (lambda (x)
	       (not (string= pname (get (get x 'pos-lit) 'name))))
	     program))

(defun var-list (clause)
  (let ((pos-literal (get clause 'pos-lit))
	;; (setq pos-literals (get clause 'pos-lit))
	(neg-literals (get clause 'neg-lits)))
    (remove-duplicates (append (mapcan (lambda (x)
					 (mapcan 'vars-in-term (get x 'args)))
				       neg-literals)
			       (mapcan 'vars-in-term (get pos-literal 'args))))))

;; replaces all variables with new ones consitently
(defun create-new-variables (clause)
  (let ((vars (var-list clause))
	;; (setq vars (var-list clause))
	)
    (apply-unifier-to-clause (mapcar (lambda (x)
				       (let ((new-var (gensym "var-")))
					 (import new-var)
					 (setf (get new-var 'type) 'variable)
					 (setf (get new-var 'name) (get x 'name))
					 (list x new-var)))
				     vars)
			     clause)
    clause))

;; calculates the resolvent of a resolution by
;;   - deleting the first neg-lit
;;   - applieing the unifier and
;;   - replasing all variables with new ones ???

;; (setq goal-clause cur-goal-clause)
;; (setq prog-clause (car cur-procedure))
(defun calc-resolvent (unifier goal-clause prog-clause)
  (let ((new-subgoals (get prog-clause 'neg-lits))
	;; (setq new-subgoals (get prog-clause 'neg-lits))
	(sym (gensym "clause-"))
	;; (setq sym (gensym "clause-"))
	)
    (import sym)
    (setf (get sym 'clause-type) 'goal-clause)
    (setf (get sym 'pos-lit) (get goal-clause 'pos-lit))
    (setf (get sym 'neg-lits) (append new-subgoals (cdr (get goal-clause 'neg-lits))))
    (create-new-variables (apply-unifier-to-clause unifier sym))
    ;; (setq clause (apply-unifier-to-clause unifier sym))
    ))

(defun next-literal (goal)
  (car (get goal 'neg-lits)))

(defun next-literal-name (goal)
  (get (next-literal goal) 'name))

;; removes the first element of the first element
(defun update-ps (progstack)
  (cons (cdr (car progstack))
	(cdr progstack)))

;; (setq test-input (parse-file "/home/steven/dev/lisp/cl-reason/test/test_prog.clr") input (multiple-value-list (split-prog-from-goals test-input)) program (car input) goal (car (cadr input)))
;; (setq input (multiple-value-list (split-prog-from-goals test-input)))
;; (setq program (car input))
;; (setq goal (car (cadr input)))

;; program is the list of prog-clauses
;; Goal is a clause with the pos-lit "answer(x1,...,xn)"
;; where xi is a variable in the goal
(defun sld-resolution (program goal)
  (do* ((goals (list (prepare-goal goal)))
	;; (setq goals (list (prepare-goal goal)))
	(cur-goal-clause (copy-clause (car goals))
			 (copy-clause (car goals)))
	;; (setq cur-goal-clause (copy-clause (car goals)))
	(cur-subgoal (car (get cur-goal-clause 'neg-lits))
		     (car (get cur-goal-clause 'neg-lits)))
	;; (setq cur-subgoal (car (get cur-goal-clause 'neg-lits)))
	(program-stack (list (mapcar 'copy-clause (extract-procedure program (get cur-subgoal 'name)))))
	;; (setq program-stack (list (mapcar 'copy-clause (extract-procedure program (get cur-subgoal 'name)))))
	)
       ((not program-stack) 'fail)
       (let ((cur-procedure (car program-stack)))
	 ;; (setq cur-procedure (car program-stack))
	 (if (not cur-procedure)
	     (progn ;; Backtracking
	       (setq goals (cdr goals))
	       (setq program-stack (cdr program-stack)))
	   (let* ((next-lit (get (car cur-procedure) 'pos-lit))
		  ;; (setq next-lit (get (car cur-procedure) 'pos-lit))
		  (unifier (unify-literals cur-subgoal next-lit))
		  ;; (setq unifier (unify-literals cur-subgoal next-lit))
		  )
	     (cond ((equal unifier 'fail)
		    (setq program-stack (update-ps program-stack)))
		   (T (let* ((resolvent
			      (calc-resolvent unifier cur-goal-clause (car cur-procedure)))
			     ;; (setq resolvent (calc-resolvent unifier cur-goal-clause (car cur-procedure)))
			     (neg-literals (get resolvent 'neg-lits))
			     ;; (setq neg-literals (get resolvent 'neg-lits))
			     )
			(if (not neg-literals)
			    (return (list (get goal 'vars) (get (get resolvent 'pos-lit) 'args)))
			  (progn (setq goals (cons resolvent goals))
				 (setq program-stack
				       (cons (mapcar 'copy-clause
						     (extract-procedure program
									(get (car (get (car goals) 'neg-lits)) 'name)))
					     (update-ps program-stack)))))))))))))

(defun run-program (path)
  (let* ((input (multiple-value-list (split-prog-from-goals (parse-file path))))
	 (program (car input))
	 (goals (cadr input)))
    (mapcar (lambda (goal)
	      (sld-resolution program goal))
	    goals)))
