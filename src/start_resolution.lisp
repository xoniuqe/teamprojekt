
(SETQ SYSTEM:*STACK-OVERFLOW-BEHAVIOUR* NIL)

 ; (if (not (require 'asdf)) 
(load (current-pathname "../lib/asdf" ))
  ;  )
 ; (require 'asdf)
(load (current-pathname "init" ))
(load (current-pathname "util/util" ))

(load (current-pathname "../lib/cl-ppcre-2.0.11/cl-ppcre.asd"))
(load (current-pathname "../lib/cl-yacc-20101006-darcs/yacc.asd"))


(load (current-pathname "parser/lexer" ))
(load (current-pathname "parser/parser" ))
(load (current-pathname "predicates/predicates" ))
(load (current-pathname "logik/resolution" ))
(load (current-pathname "helper/helper" ))

(load (current-pathname "gui/GUI"))
  

(defun clr-main () 
  (predicates:setup-predicates )
  (setf folder (directory (current-pathname "../predicates/*.*" *load-truename*)))
  (predicates:load-predicates folder)
  (set-eval-func 'run)
  (main)
)


; zeug zum testen und für schöne ausgabe
;(setq path (merge-pathnames "../test/uebung1.clre" *load-truename*))
;(setq path (merge-pathnames "../test/test_number.clre" *load-truename*))
;(setq path (merge-pathnames "../test/test.clre" *load-truename*))



;(setq path Pfad)
(defun run (path) 
  (setq liste (first (resolution:run-program path)))
  (setf result (mapcar (lambda(x) 
	(mapcar 'helper:calc-result x)
	) liste)) 
)
