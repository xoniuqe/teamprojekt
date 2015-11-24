(cd #P"C:/Users/Tobias/Documents/Lisp/cl-reason")


(require 'asdf)

(pushnew "registry/" asdf:*central-registry* :test #'equal)
(load "registry/cl-ppcre/cl-ppcre.ofasl")
(load "registry/yacc/yacc.asd")
(asdf:load-system :cl-ppcre)
(asdf:load-system :yacc)

(load "cl-reason.asd")

(asdf:load-system :cl-reason)


(setq result (cl-reason:run-program "test_prog_2.clr"))
(setq liste (first  result))
;(write liste)
(mapcar (lambda (x) 
        ; (write (symbol-plist (first x)))
          (get (first x) 'cl-reason::name)
)

         
liste)