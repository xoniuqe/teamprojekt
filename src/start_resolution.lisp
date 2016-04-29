
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
  ;(predicates:setup-predicates )
 ; (setf folder (directory (current-pathname "../predicates/*.*" *load-truename*)))
 ; (predicates:load-predicates folder)
  (set-eval-func 'run)
  (main)
)


; zeug zum testen und für schöne ausgabe
;(setq path (merge-pathnames "../test/uebung1.clre" *load-truename*))
;(setq path (merge-pathnames "../test/test_number.clre" *load-truename*))
;(setq path (merge-pathnames "../test/test_pred3.clr" *load-truename*))


(predicates:setup-predicates )
(setf folder (directory (current-pathname "../predicates/*.*" *load-truename*)))
(predicates:load-predicates folder)


;(setq path Pfad)
(defun run (path) 

  (setq liste (first (resolution:run-program path)))
  (setf result (mapcar (lambda(x) 
                        (listp x)
	(mapcar 'helper:calc-result x)
	) liste)) 
 ; (print result)
   (setf output nil)
  (if (and (not (listp (first result))) (first result)) (setf output "True") 
;(setq var (car (first result)))
;(setq value (car (second result)))

  (do* ((var (car (first result)) (car (first result)))
        (value (car (second result)) (car (second result))))
       ((not (first result)))

       ;(format nil "~{~a~^, ~}" value)

        (let ((firstl (cdr (first result)))
              (secondl (cdr (second result))))
       (setf result firstl)
       (setf result (list result secondl)))
        (print value)
       ;(setf output (format t "~S\n~S=~S" output var value))^
        (if (listp value) (setf value (concatenate 'string "[" (list-to-string value) "]")))
        (print value)
        (setf output (append output (list (concatenate 'string var "=" value))))
 ))
   output
)
(setq list '(1 2 3 "a" "b" "c" "empty") )
(defun list-to-string (list)
	(setf elements  (- (length list) 2))
	(setf r (first list))
        (if (numberp r) (setf r (write-to-string r)))
	(if (not (listp list)) (return list))
       ; (setq i 1)
	(do ((i 1 (+ i 1)))
		((> i elements ))
                (setf elem (nth i list))
                (if (numberp elem) (setf elem (write-to-string elem)))
		(setf r (concatenate 'string r ", " elem))
	)
	r
)