;(cd #P"C:/Users/Tobias/Documents/Lisp/cl-reason/src/")
;(get-current-directory)
#|
(load (merge-pathnames "../lib/asdf" *load-truename*))

(load (merge-pathnames "init" *load-truename*))
(load (merge-pathnames "util/util" *load-truename*))
(load (merge-pathnames "parser/lexer" *load-truename*))
(load (merge-pathnames "parser/parser" *load-truename*))
(load (merge-pathnames "predicates/predicates" *load-truename*))
(load (merge-pathnames "logik/resolution" *load-truename*))



; zeug zum testen und für schöne ausgabe

(setq file-path (merge-pathnames "../test/test_pred3.clr" *load-truename*))

(predicates:setup-predicates )
(setq folder (directory (merge-pathnames "../predicates/*.pred" *load-truename*)))
(predicates:load-predicates folder)

(setq liste (first (resolution:run-program file-path)))

;(let ((in (open file-path :if-does-not-exist nil)))
; (when in
;    (loop for line = (read-line in nil))
;         (while line do (write-line line))
;     (close in)))

(mapcar (lambda(x) 
(mapcar (lambda (y) (get y 'resolution::name)) x)
) liste)

|#