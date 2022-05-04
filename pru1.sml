(* pruebas *)

val f = constante false
val t = constante true

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
val prop2 = f :=>: f :<=>: ~: f :||: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p

val prop5 = t :=>: f :<=>: ~: p :||: q
val prop6 = p :=>: q :<=>: ~: p :=>: ~: q
;


evalProp prop1;
evalProp prop2;
evalProp prop3;
evalProp prop4;
evalProp prop5;
evalProp prop6;

constante 30;

val it = ();
vars pru1;

val booleanos = gen_bools 2;
exception Head;
fun head[] = raise Head | head (x :: xs) = x ;
val contexto = as_vals (vars pru1)(head booleanos);
evalProp contexto pru1;
val contexto = as_vals las_vars [true,false];
evalProp contexto pru1;