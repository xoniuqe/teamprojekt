(asdf:defsystem clr
  :version "0.0.1"
  :depends-on ("yacc" "cl-ppcre")
  :components ((:file "start_resolution")
               (:file "init")
			   (:file "gui\gui" :depends-on "helper\helper)
			   (:file "helper\helper")
			   (:file "logik\resultion")
			   (:file "parser\parser")
			   (:file "parser\lexer")
			   (:file "predicates\predicates")
			   (:file "util\util")
			  )