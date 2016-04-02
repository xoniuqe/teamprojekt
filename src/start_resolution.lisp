;(cd #P"C:/Users/Tobias/Documents/Lisp/cl-reason/src/")

(load "init")
(load "util/util")
(load "parser/lexer")
(load "parser/parser")
(load "predicates/predicates")
(load "logik/resolution")



; zeug zum testen und für schöne ausgabe

 (setq file-path "../test/test_pred2.clr")

(setq liste (first (resolution:run-program file-path)))

;(let ((in (open file-path :if-does-not-exist nil)))
; (when in
;    (loop for line = (read-line in nil))
;         (while line do (write-line line))
;     (close in)))

(mapcar (lambda(x) 
;(list 
(get (first x) 'resolution::name)
;(get (second x) 'resolution::name)
;)
) liste)

(setq pred (resolution:predicates-test))
(get pred 'resolution::name)
(funcall pred "test")

(setf func (get pred 'resolution::func))

(eval 'func)

(let (sym (gensym "bla"))

(setf (symbol-function sym) (predicates:load-predicate "../predicates/issteven.pred"))
(funcall (eval test) "steven")

)