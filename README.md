# cl-reason
A reasoning system for horn logik developed in common lisp

GUI extension and support for predefined predicates.

## Predefined predicates
The predefined predicates are realised with lisp-lambda-functions, but can also be written with the project own horn-logic-languague


## Example program

forall a b x y: demorgan(nicht(und(a,b)), oder(x,y)) <= demorgan(nicht(a),x) and demorgan(nicht(b),y).
forall a b x y: demorgan(nicht(oder(a,b)), und(x,y)) <= demorgan(nicht(a),x) and demorgan(nicht(b),y).
forall a r: demorgan(nicht(nicht(a)), r) <= demorgan(a,r).
forall a b x y: demorgan(und(a,b), und(x,y)) <= demorgan(a,x) and demorgan(b,y).
forall a b x y: demorgan(oder(a,b), oder(x,y)) <= demorgan(a,x) and demorgan(b,y).
forall x: demorgan(x,x).

exists x : demorgan(und(nicht(nicht(a)),nicht(b)),x).

Program that shows the demorgan rules
