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


(predicates:setup-predicates )
(predicates:get-predicate "iststeven")
(setf issteven (predicates:get-predicate-func "iststeven"))
(apply issteven '("steven"))
(predicates:get-predicate "bla")
;(predicates:is-predefined-predicate2 "iststeven")

(predicates:load-predicate "../predicates/issteven.pred") 

(setq path (pathname "../predicates/issteven.pred"))
(dateilesen path)

(defun dateilesen (dateiname)
  (do* ((streamin (open dateiname))
        exprs
        (expr (read streamin nil 'eof)
              (read streamin nil 'eof)))
       ((equal expr 'eof) (close streamin)
        (nreverse exprs))
    (setq exprs (cons expr exprs)))) 

(dataread "../predicates/issteven.pred")
(defun dataread (name)
  (defparameter test "")
  (do* ((in (open name))
        lines 
        (line (read-line in nil 'eof)
              (read-line in nil 'eof)))
       ((equal line 'eof) (close in))
    (setq test (setq lines (concatenate 'string lines line (make-string 1 :initial-element #\newline))))))
(print test)

(print (dateilesen "../predicates/issteven.pred"))