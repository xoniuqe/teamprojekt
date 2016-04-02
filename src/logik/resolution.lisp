(defpackage :resolution
  (:use :common-lisp :util :parser :lexer :predicates)
  (:export :unify)
  (:export :run-program)
  (:export :predicates-test)
  )

(in-package :resolution)

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
;(print "unify-literals")
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
	;(print "extract-procedure")
	;(print program)
	;(print pname)
	;
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
;(print "calc-resolvent")
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
	;(print "update-ps")
  (cons (cdr (car progstack))
	(cdr progstack)))

;; (setq test-input (parse-file "/home/steven/dev/lisp/cl-reason/test/test_prog.clr") input (multiple-value-list (split-prog-from-goals test-input)) program (car input) goal (car (cadr input)))
;; (setq input (multiple-value-list (split-prog-from-goals test-input)))
;; (setq program (car input))
;; (setq goal (car (cadr input)))

(defun check-for-predefined-predicates (cur-procedure)
	(let* ((pos-lit (get (first cur-procedure) 'parser:pos-lit))
		   (neg-lits (get (first cur-procedure) 'parser:neg-lits))
		 ;  (predefined (remove-if-not 'is-predefined-predicate neg-lits))
		  ; (evaluated (remove-if-not 'eval-predefined-predicate predefined))
		)
	
		;(setf without-predicates (remove-if #'is-predefined-predicate neg-lits))

		;(setf (get (first cur-procedure) 'parser:neg-lits) without-predicates)

		;(setf returnv (if (eq (list-length predefined) (list-length evaluated)) cur-procedure NIL))
		;(print (first (get pos-lit 'lexer:args)))
		;(print pos-lit)
		;(print "is const:")
		;(print (term-is-const(first (get pos-lit 'lexer:args))))
		
	;	(setq neg-lits (remove-if (
	;		lambda (lit) 
	;			(if (and (is-predefined-predicate lit) (term-is-const (first (get lit 'lexer:args))))
	;				(if (not (eval-predicate (get lit 'lexer:name)  (first (get lit 'lexer:args))))
	;					NIL
	;					T
	;				)
	;			NIL)
	;	) neg-lits))
	;	(setf (get (first cur-procedure) 'parser:neg-lits) neg-lits)
	;	
	;	(if (and (is-predefined-predicate pos-lit) (term-is-const (first (get pos-lit 'lexer:args))))
	;		(if (not (eval-predicate (get pos-lit 'lexer:name)  (first (get pos-lit 'lexer:args))))
	; 		(return-from check-for-predefined-predicates nil)
	;			(setf (get pos-lit 'parser:pos-lit) nil)
;			)
		;	(print "no predicate or const")
	;	)
	;	(print "returning")
		;(print cur-procedure)
		(setq tmp (remove-if #'is-predefined-predicate neg-lits))
		;(print "TMP")
		;(print tmp)
		;(print (get (first cur-procedure) 'parser:neg-lits))
		(setf (get (first cur-procedure) 'parser:neg-lits) tmp)
		(if (is-predefined-predicate pos-lit)
			(setf (get (first cur-procedure)  'parser:pos-lit) nil))
		(return-from check-for-predefined-predicates cur-procedure)
	)
)





(defun check-eval-predefined-predicate(lit)
	;(print "check-eval")
	;;(if (and (is-predefined-predicate lit) (eval-predefined-predicate lit) T NIL))
	(if (is-predefined-predicate lit) (if (eval-predefined-predicate lit) T NIL) T)
)

(defun is-predefined-predicate (lit)
	;(print "is-predefined-predicate")
	(cond ((string= (get lit 'lexer:name) "iststeven") T)
		  ((string= (get lit 'lexer:name) "print") T)
		  (T NIL))
)

(defun eval-predicate (pred-name literals)
	;(print "eval-predicate");;demo
	;(print literals)
	(setq check-value (first literals)) ;; demo
	(cond 
		((and (string= pred-name "iststeven") (string-equal (get check-value 'lexer:name) "steven")) T)
		((string= pred-name "print") (progn (print (get check-value 'lexer:name)) T))
		(T NIL)
	)
)

(defun eval-predefined-predicate (lit)
	(if ( and (string= (get lit 'lexer:name) "iststeven") (string=  (get (get lit'lexer:var) 'lexer:name) "steven")) T NIL)
	(if (string= (get lit 'lexer:name) "print") (progn (print  (get (get lit'lexer:var) 'lexer:name)) T) NIL)
	;(cond ((not lit) NIL) (T T))
)

(defun predicates-test () 
	(let ((sym (gensym "PRED-")))
		(import sym)
		(setf (get sym 'name) "issteven")
		(setf (symbol-function sym) (lambda (x) (string= x "steven")))
		sym
	)
)

(defun literal-list-is-const (literals)
	(remove-if (lambda (lit) (term-is-const (first (get lit 'lexer:args)))) literals)
)

;  (setq path "../test/test_pred.clr")
; (setq input (multiple-value-list (split-prog-from-goals (parse-file path))))
; (setq program (car input))
; (setq goal (first (cadr input)))


;; program is the list of prog-clauses
;; Goal is a clause with the pos-lit "answer(x1,...,xn)"
;; where xi is a variable in the goal
(defun sld-resolution (program goal)
;(print program)
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
	 ;  (print program-stack)
       (let ((cur-procedure (car program-stack)))
;Hier wird gepr�ft ob die aktuelle Klausel ein Pr�dikat ist, dazu wird, das pos-lit genommen und gepr�ft ob es zu den vordefinierten pr�dikaten geh�rt
		;(print program-stack)
		;(print cur-procedure)	
		;(print "next clause")
		;(setq cur-procedure (check-for-predefined-predicates cur-procedure))

	 ;; (setq cur-procedure (car program-stack))
	 (if (not cur-procedure)
	     (progn ;; Backtracking
	       (setq goals (cdr goals))
	       (setq program-stack (cdr program-stack))
	;	(print "back-tracking")
)
 
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
				 ;(print neg-literals)
	;; hiermit wird das program auch dann beendet wenn nur diese eine clausel auf das prädikat zutrifft
				(setf neg-literals (remove-if (lambda (lit) 
				;;lexer:args enthält eine liste der literale, diese müssen mit einer schleife getestet werden ob jedes const ist
				;; dann mus diese liste an eval-predicate gereicht werden
				(if (is-predefined-predicate lit);(and (is-predefined-predicate lit) (term-is-const (first (get lit 'lexer:args))))
					(if (not (eval-predicate (get lit 'lexer:name)   (get lit 'lexer:args)))
						NIL
						T
					)
				NIL)) neg-literals))
				
			;	(print "neg-literals")
			;	(print neg-literals)
	;			 (print "resolvente:")
		;		 (print resolvent)
			(if (not neg-literals)
			    (return (list (get goal 'vars) (get (get resolvent 'pos-lit) 'args)))
			  (progn (setq goals (cons resolvent goals))
			;	(print "resoliertes goal")
			;	(print goals)
				 (setq program-stack
				       (cons (mapcar 'copy-clause
						     (extract-procedure program
									(get (car (get (car goals) 'neg-lits)) 'name))) ;;nächste clausel wird ausgewählt, nach dem namen des ersten negativen literals des (resolierten) ziels
					     (update-ps program-stack)))))))))))))

(defun run-program (path)
  (let* ((input (multiple-value-list (split-prog-from-goals (parse-file path))))
	 (program (car input))
	 (goals (cadr input)))
    (mapcar (lambda (goal)
	      (sld-resolution program goal))
	    goals)))
		


;;; (run-program "/home/steven/dev/lisp/cl-reason/test/test_prog.clr")
;;; (run-program "/home/steven/dev/lisp/cl-reason/test/test_prog_2.clr")
;;; (run-program "/home/steven/dev/lisp/cl-reason/test/test_prog_3.clr")

