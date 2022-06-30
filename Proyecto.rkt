#lang eopl

;; Proyecto final
;;Grupo: 01.

;;Integrantes
;;#. Nombres completos            -   Código estudiantil
;;1. Sebastián Tutistar Valencia           2110309
;;2. Mavelyng Sterling Londoño             1430871
;;3. Jefersson Danilo Arévalo              1926167

;Definición Backus-Naur form (BNF) para las expresiones del lenguaje:
;
;  <program>       ::= <expression>
;                      <a-program (exp)>
;  <expression>    ::= <number>
;                      <lit-exp (datum)>
;                  ::= <identifier>
;                      <var-exp (id)>
;                  ::= <primitive> ({<expression>}*(,))
;                      <primapp-exp (prim rands)>
;                  ::= if <expresion> then <expresion> else <expression>
;                      <if-exp (exp1 exp2 exp23)>
;                  ::= let {identifier = <expression>}* in <expression>
;                      <let-exp (ids rands body)>
;                  ::= proc({<identificador>}*(,)) <expression>
;                      <proc-exp (ids body)>
;                  ::= (<expression> {<expression>}*)
;                      <app-exp proc rands>
;  <primitive>     ::= + | - | * | add1 | sub1 


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
  )
)

;Especificación Sintáctica (Gramática)
