(in-package :cl-user)

(defpackage :cl-reason-asd
  (:use :common-lisp :asdf :cl-ppcre :yacc)
  )

(in-package :cl-reason-asd)

(defsystem "cl-reason"
	:name "cl-reason"
    :components ((:file "packages")
				 (:file "cl-reason")))
