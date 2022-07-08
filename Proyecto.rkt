#lang eopl

;; Proyecto final
;; Grupo: 01.

;;Integrantes
;;#. Nombres completos            -   Código estudiantil
;;1. Sebastián Tutistar Valencia           2110309
;;2. Mavelyng Sterling Londoño             1430871
;;3. Jefersson Danilo Arévalo              1926167

;Definición Backus-Naur form (BNF) para las expresiones del lenguaje:
;
;  <program>       ::= <expression>
;                      <a-program (exp)>
;  <expresion>    ::= <number> (numero)
;                  ::= <lit-exp (datum)>
;                  ::= <identifier>
;                      <var-exp (id)>
;                  ::= <valor-true> true
;                  ::= <valor-false> false
;                  ::= <empty-exp> null
;                  ::= <primitive> ({<expresion>}*(,))
;                      <primapp-exp (prim rands)>
;                  ::= (primitiveString-exp) [<expresion> <primitive-str>]
;                  ::= <caracter>
;                      <caracter-exp>
;                  ::= <cadena>
;                     <cadena-exp>
;                  ::= begin {<expresion>}+(;) end
;                   ::= if (<expresion>) "{" <expresion>"}" else "{"<expresion>"}"
;                      <if-exp (exp1 exp2 exp23)> 
;                  ::= while (<expresion>) "{" <expresion>"}"
;                      <expresion> done
;                   ::= for (<identifier> = <expresion> <to> <expresion>) "{" <expresion>"}"
;                      <expresion> { <program>} fin
;                  ::= let {identifier = <expresion>}* in <expresion>
;                      <let-exp (ids rands body)>
;                  ::= rec  {identifier ({identificador}(,)) = <expresion>} in <expresion>
;                      <rec-exp proc-names idss bodies bodyrec>
;                  ::= proc({<identificador>}*(,)) <expresion>
;                      <proc-exp (ids body)>
;                  ::= (bas-8) bas8 <expresion> <bas8-exp> <expresion>;
;                  ::= (bas-16) bas16 <expresion> <bas16-exp> <expresion>;
;                  ::= (bas-32) bas32 <expresion> <bas32-exp> <expresion>;
;                  ::= (<expresion> {<expresion>}*)
;                       <app-exp proc rands>
;                  := (hex-number) (hex number {, number}*)
;                  := (oct-number) (oct number {, number}*)
;
; <lista>          ::= [{<expresion>} * (;)]
; <vector>         ::= vector[{<expresion>} * (;) ]
; <registro>       ::= { {<identificador> = <expresion> } + (;) }
; <expr-bool>      ::= <pred-prim>(<expresion> , <expresion>)
;                      <oper-bin-bool>(<expr-bool >, <expr-bool>)
;                  ::= <bool>
;                  ::= <oper-un-bool>(<expr-bool>)
;
; <primitive>      ::= + | - | * | add1 | sub1
;
; <pred-prim>      ::= < | <= | > | >= | == | != | && | || | <>
;
; <oper-bin-bool>  ::=  and | or 
; <oper-un-bool>   ::= (not-bool) not


;  <primitive-8>   ::= (suma8) +x8
;                  ::= (resta8) -x8
;                  ::= (multip8) *x8
;                  ::= (add16) ++x8
;                  ::= (rest16) --x8
;                  

;  <primitive-16>  ::= (suma16) +x16
;                  ::= (resta16) -x16
;                  ::= (multip16) *x16
;                  ::= (add16) ++x6
;                  ::= (rest16) --x6

;  <primitive-32>  ::= (suma32) +x32
;                  ::= (resta32) -x32
;                  ::= (multip132) *x32
;                  ::= (add32) ++x32
;                  ::= (rest32) --x32

; <primitive-str>  ::= (len-string) .length
; <primitive-list> ::= (list-prim) list


;Especificación Léxica

(define especificacion-lexica
'((espacio-en-blanco (whitespace) skip)
  (comentario ("/*" (arbno (not #\newline))) skip)
  (identificador ((or letter "_" "$")(arbno (or letter digit "-" ))) symbol)
  (nombre ("@" letter (arbno letter)) string)
  (texto ("'" letter (arbno (or letter digit)) "'") string)
  (var-null ("null") string)
  (numero (digit (arbno digit)) number) 
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (caracter ("\'" letter "\'" ) symbol)
  (cadena ("\""(or letter whitespace digit) (arbno (or whitespace letter digit)) "\"") string)
  )
)

;Especificación Sintáctica (gramática)

(define gramatica

  '(
    (program (expresion) a-programa)

    ;;Const-exp: basado en Java,comienzan con cons identificador = expresion.
    (expresion-def ("const" identificador "=" expresion) const-exp)

    ;;Var-exp: basado en Java, comienzan con var identificador = expresion
    (expresion-def ("var" identificador "=" expresion)  var-exp)

    ;;Rec-exp: basadado en Java, comienzan con rec identificador = expresion
    (expresion ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion)  "in" expresion) rec-exp)

    ;;Identificador: Basado en Java, pueden comenzar con letras| "_" | "$"
    (expresion (identificador) var-exp)

    ;;Referencia 
    (expresion ("&" identificador) refe-exp)

    ;;Caracter: Basado en Java
    (expresion (caracter) caracter-exp)

    ;;Cadena: Basado en Java
    (expresion (cadena) cadena-exp)

    ;;Valor-empty: Basado en Java, se define las variables sin valor con null
    (expresion (valor-empty) empty-exp)
    
    ;;Número: Basado en Java
    (expresion (numero) number)

    ;;Número en Base 8: Basado en Java
    (expresion ("('x8 " (arbno numero) ")") bas8-exp)
    
    ;;Número en Base 16: Basado en Java
    (expresion ("('x16 " (arbno numero) ")") bas16-exp)

    ;;Número en Base 32: Basado en Java
    (expresion ("('x32 " (arbno numero) ")") bas32-exp)

    ;;Booleanos: Basado en Java (True y False)
    (expresion ("true") valor-true)
    (expresion ("false") valor-false)

    ;;Crear-lista: basado en Java
    (expresion ("crear-lista" "(" expresion (arbno "," expresion) ")") lista-exp)
    
    ;;Crear-vector: basado en Java
    (expresion ("vector ("(separated-list expresion ",") ")") vector-exp)
    
    ;;Crear-registro: basado en Java
    (expresion ("crear-registro ("(separated-list identificador "->" expresion ";") ")")crear-registro-exp)

    ;;Estructuras de Control
    (expresion ("begin" expresion ";" (arbno ";" expresion)"end") begin-exp)
    (expresion ("if" "(" expresion")" "{" expresion "}" "else" "{" expresion "}") if-exp)
    (expresion ("while" "("expresion")" "{"expresion"}" ) while-exp)
    (expresion ("for" "(" identifier "=" expresion ";" to expresion ")" "{" expresion"}") for-exp)
    
    ;;Expresiones para números hexadecimales
    (expresion ("bas8" expresion primitive-8 expresion ";") bas8-exp)
    (expresion ("bas16" expresion primitive-16 expresion ";") bas16-exp)
    (expresion ("bas32" expresion primitive-32 expresion ";") bas32-exp)
    
    ;;Primitivas para los enteros: Basado en Java
    (primitive ("+") sum-ent)
    (primitive ("-") rest-ent)
    (primitive ("*") mult-ent)
    (primitive ("%") mod-ent)
    (primitive ("/") div-ent)
    (primitive ("add1") incre-ent)
    (primitive ("sub1") decre-ent)
    
    ;;Primitivas predifinidos: Basado en Java
    (pred-prim (">") mayor-bool)
    (pred-prim (">=") mayor-igual-bool)
    (pred-prim ("<") menor-bool)
    (pred-prim ("<=") menor-igual-bool)
    (pred-prim ("==") igual-bool)
    (pred-prim ("!=") diferente-bool)
    (pred-prim ("&&") and-bool)
    (pred-prim ("||") or-bool)
    
    ;;Primitivas para base 8: Basado en Java
    (primitive-8 ("+8") suma8)
    (primitive-8 ("-8") resta8)
    (primitive-8 ("*8") multip8)
    (primitive-8 ("++8") add8)
    (primitive-8 ("--8") sub8)
    
    ;;Primitivas para base 16: Basado en Java
    (primitive-16 ("+16") suma16)
    (primitive-16 ("-16") resta16)
    (primitive-16 ("*16") multip16)
    (primitive-16 ("++16") add16)
    (primitive-16 ("--16") sub16)
    
    ;;Primitivas para base 32: Basado en Java
    (primitive-32 ("+32") suma32)
    (primitive-32 ("-32") resta32)
    (primitive-32 ("*32") multip32)
    (primitive-32 ("++32") add32)
    (primitive-32 ("--32") sub32)
    
    ;;Primitiva string para la longitud y concatenar
    (primitive ("longitud") prim-longitud)
    (primitive ("concatenar") prim-concatenar)
    
      )
    
    )
