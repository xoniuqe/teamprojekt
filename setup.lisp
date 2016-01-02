(cd #P"C:/Users/Tobias/Documents/Lisp/cl-reason")


(require 'asdf)

(pushnew "registry/" asdf:*central-registry* :test #'equal)
(load "registry/cl-ppcre/cl-ppcre.ofasl")
(load "registry/yacc/yacc.ofasl")
(asdf:load-system :cl-ppcre)
;(asdf:load-system :yacc)

 (load "cl-reason.asd")

 (asdf:load-system :cl-reason)
(setq file-path "test_prog_3.clr")

(setq liste (first (cl-reason:run-program file-path)))

 (let ((in (open file-path :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (write-line line))
    (close in)))

(mapcar (lambda (x) 
        ; (write (symbol-plist (first x)))
          (get (first x) 'cl-reason::name)
)

         
liste)




