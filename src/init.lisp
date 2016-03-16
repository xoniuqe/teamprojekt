
#-asdf (load  "../lib/asdf")
(pushnew "../registry/" asdf:*central-registry* :test #'equal)
(pushnew "../lib/cl-ppcre-2.0.9/" asdf:*central-registry* :test #'equal)
(pushnew "../lib/cl-yacc-20101006-darcs/" asdf:*central-registry* :test #'equal)