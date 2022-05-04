(* Lenguaje de proposiciones con constantes. No tiene variables *)

(* Aqui definimos la sintaxis abstracta de nuestro pequenno
   lenguaje de proposiciones con constantes *)

datatype PropConst =
  constante    of bool 
| variable     of string
| negacion     of PropConst 
| conjuncion   of PropConst * PropConst
| disyuncion   of PropConst * PropConst
| implicacion  of PropConst * PropConst
| equivalencia of PropConst * PropConst
;

fun imprimir prop = 
  case prop of  
    constante false             => "false"
  | constante true              => "true"
  | variable nombre             => nombre
  | negacion prop1              => "negación (" ^ imprimir prop1 ^ ")"
  | conjuncion (prop1, prop2)   => "conjunción (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | disyuncion (prop1, prop2)   => "disyunción (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | implicacion (prop1, prop2)  => "implicación (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | equivalencia (prop1, prop2) => "equivalencia (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
 ; 

 fun imprimir2 prop = 
  case prop of  
    constante false             => "false"
  | constante true              => "true"
  | variable nombre             => nombre
  | negacion prop1              => "negación (" ^ imprimir prop1 ^ ")"
  | conjuncion (prop1, prop2)   => "conjunción (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | disyuncion (prop1, prop2)   => "disyunción (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | implicacion (prop1, prop2)  => "implicación (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
  | equivalencia (prop1, prop2) => "equivalencia (" ^ imprimir prop1 ^ "," ^ imprimir prop2 ^ ")"
 ; 


nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infixr 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

infix 4 :++:
val (op :++:) = fn (p, q) => ~: (p :<=>: q)

;
val F = constante false: PropConst;
val V = constante true: PropConst;
V :&&: F;

val pru1 = (variable "a") :&&: (variable "b");
val las_vars = vars pru1;
 
booleanos;
evalProp (as_vals las_vars (head booleanos));
