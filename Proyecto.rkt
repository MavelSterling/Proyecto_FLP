#lang eopl

;; Proyecto final
;; Grupo: 01.

;;Integrantes
;;#. Nombres completos            -   Código estudiantil
;;1. Sebastián Tutistar Valencia           2110309
;;2. Mavelyn Sterling Londoño             1430871
;;3. Jefersson Danilo Arévalo              1926167

;Definición Backus-Naur form (BNF) para las expresiones del lenguaje:
;
;  <program>       ::= <expresion>
;                      <a-program (exp)>
;  <expresion>    ::= <number> (numero)
;                  ::= <lit-exp (datum)>
;                  ::= <identificador>
;                      <var-exp (id)>
;                  ::= <valor-true> true
;                  ::= <valor-false> false
;                  ::= <empty-exp> null
;                  ::= <primitive> ({<expresion>}*(,))
;                      <primapp-exp (prim rands)>
;                  ::= <caracter>
;                      <caracter-exp>
;                  ::= <cadena>
;                     <cadena-exp>
;                  ::= begin {<expresion>}+(;) end
;                   ::= if (<expresion>) "{" <expresion>"}" else "{"<expresion>"}"
;                      <if-exp (exp1 exp2 exp23)> 
;                  ::= while (<expresion>) "{" <expresion>"}"
;                      <expresion> done
;                   ::= for (<identificador> = <expresion> <to> <expresion>) "{" <expresion>"}"
;                      <expresion> { <program>} fin
;                  ::= let {identificador = <expresion>}* in <expresion>
;                      <let-exp (ids rands body)>
;                  ::= rec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
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

;<expresion> ::= FNC <numero> ( clausula-or )+ (”and”)
;<clausula-or> ::= ( <numero> )+ (”or”)


;Especificación Léxica

(define especificacion-lexica
'((espacio-en-blanco (whitespace) skip)
  (comentario ("/*" (arbno (not #\newline))) skip)
  (identificador(letter (arbno (or letter digit))) symbol)
  (nombre ("@" letter (arbno letter)) string)
  (numero (digit (arbno digit)) number) 
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (caracter ("'"letter"'") symbol)
  (cadena ("$"(or letter whitespace digit) (arbno (or whitespace letter digit))) string)
  )
)

;Especificación Sintáctica (gramática)

(define gramatica

  '(
    (program (expresion) a-programa)
    
    ;Definiciones:

    ;var: basado en Java, comienzan con var identificador = expresion
    (expresion ("var" identificador "=" expresion)  var-exp)

    ;const: basado en Java,comienzan con cons identificador = expresion
    (expresion ("const" identificador "=" expresion) const-exp)

    ;rec: basado en Java, comienzan con letrec identificador = expresion
    (expresion ("rec" (arbno nombre "(" (separated-list nombre ",") ")" "=" expresion)  "in" expresion) rec-exp)
    
    ;Datos

    ;identificador: basado en Java
    (expresion (identificador) ide-exp)

    ;numero: basado en Java
    (expresion (numero) num-exp)

    ;caracter: basado en Java
    (expresion (caracter) caracter-exp)

    ;cadena: basado en Java
    (expresion (cadena) cadena-exp)
    
    ;Constructores de Datos Predefinidos

    ;primitiva: basado en Java, forma de escribir una primitiva.
    (expresion ("[" primitiva (separated-list expresion ",") "]") primitiva-num)

    ;lista: basado en Java
    (expresion ("lista" "(" expresion (arbno "," expresion) ")") lista-exp)
    
    ;vector: basado en Java
    (expresion ("vector" "{"(separated-list expresion ",") "}") vector-exp)
    
    ;registro: basado en Java
    (expresion ("registro" "(" (separated-list identificador "->" expresion ";") ")") registro-exp)

    ;expresiones booleanas: basadas en Java
    (expresion (pred-prim "(" expresion "," expresion ")") pred-prim-bool)
    (expresion (oper-bin-bool "(" expresion "," expresion")") oper-bin)
    (expresion (bool) bool-expr-bool)
    (expresion (oper-un-bool"(" expresion")") oper-un)

    ;primitivas booleanas: basadas en Java
    (pred-prim (">") mayor-bool)
    (pred-prim (">=") mayor-igual-bool)
    (pred-prim ("<") menor-bool)
    (pred-prim ("<=") menor-igual-bool)
    (pred-prim ("==") igual-bool)
    (pred-prim ("!=") diferente-bool)

    ;primitivas binarias booleanas: basadas en Java
    (oper-bin-bool ("and") and-boolean-primitive)
    (oper-bin-bool ("or")  or-boolean-primitive)

    ;primitiva not booleana: basado en Java
    (oper-un-bool ("not") not-boolean-primitive)

    ;primitivas bool
    (bool ("true") true->boolean)
    (bool ("false") false->boolean)
    
    ;Estructuras de Control

    ;begin: basado en Java
    (expresion ("begin" expresion ";" (separated-list expresion ";")"end") begin-exp)

    ;if: basado en Java
    (expresion ("if" "(" expresion")" "{" expresion "}" "else" "{" expresion "}") if-exp)

    ;while: basado en Java
    (expresion ("while" "("expresion")" "do" "{"expresion"}" ) while-exp)

    ;for: basado en Java
    (expresion ("for" identificador "=" expresion "to" expresion "do" "{"expresion"}" "end") for-exp)

    ;Procedimientos
    
    ;let: basado en Java
    (expresion ("let" identificador "=" expresion) let-exp)
  
    ;proc: basado en Java
    (expresion ("proc" "("(separated-list identificador ",") ")" "{" expresion "}") procedure-exp)
    
    ;invocar: basado en Java
    (expresion ("invocar" expresion "(" (separated-list expresion ",") ")") procedure-call-exp)
    
    ;Primitivas aritméticas para enteros

    (primitiva ("+") primitiva-sum)
    (primitiva ("-") primitiva-rest)
    (primitiva ("*") primitiva-mult)
    (primitiva ("%") primitiva-mod)
    (primitiva ("/") primitiva-div)
    (primitiva ("add1") incr-prim)    
    (primitiva ("sub1") decr-prim)

    ;Primitivas aritméticas para hexadecimales

    ;primitivas para base 8: basado en Java
    (primitiva-8 ("+8") suma8)
    (primitiva-8 ("-8") resta8)
    (primitiva-8 ("*8") multip8)
    (primitiva-8 ("++8") add8)
    (primitiva-8 ("--8") sub8)

    ;primitivas para base 16: basado en Java
    (primitiva-16 ("+16") suma16)
    (primitiva-16 ("-16") resta16)
    (primitiva-16 ("*16") multip16)
    (primitiva-16 ("++16") add16)
    (primitiva-16 ("--16") sub16)
    
    ;primitivas para base 32: basado en Java
    (primitiva-32 ("+32") suma32)
    (primitiva-32 ("-32") resta32)
    (primitiva-32 ("*32") multip32)
    (primitiva-32 ("++32") add32)
    (primitiva-32 ("--32") sub32)

    ;número en base 8: basado en Java
    (expresion ("('x8 " (arbno numero) ")") bas8-exp)
    
    ;número en base 16: basado en Java
    (expresion ("('x16 " (arbno numero) ")") bas16-exp)

    ;número en base 32: basado en Java
    (expresion ("('x32 " (arbno numero) ")") bas32-exp)

    ;expresiones para números hexadecimales (8,16,32)
    (expresion ("base8" expresion primitiva-8 expresion ";") bas8-exp1)
    (expresion ("base16" expresion primitiva-16 expresion ";") bas16-exp1)
    (expresion ("base32" expresion primitiva-32 expresion ";") bas32-exp1)
    
    ;Primitivas sobre cadenas
    
    ;longitud: basado en Java
    (primitiva ("longitud") prim-longitud)

    ;concatenar: basado en Java
    (primitiva ("concatenar") prim-concatenar)

    ;Primitivas sobre listas

    (primitiva ("vacio?") lst-vacio?)
    (primitiva ("vacio") lst-vacio)
    (primitiva ("lista?") lst-lista?)
    (primitiva ("cabeza" "(" expresion ")") lst-cabeza)
    (primitiva ("cola" "(" expresion ")") lst-cola)
    (primitiva ("append") lst-append)
    
    

    
;Construcciones Automáticas

(sllgen:make-define-datatypes especificacion-lexica gramatica)

;Test
(define show-the-datatypes
 (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica gramatica)))

(show-the-datatypes)

;-------------------------------------------------------
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
 (sllgen:make-string-parser especificacion-lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
   (sllgen:make-string-scanner especificacion-lexica gramatica))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
