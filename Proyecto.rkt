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

;###########################################################

;Especificación Léxica

(define especificacion-lexica
'((espacio-en-blanco (whitespace) skip)
  (comentario ("/*" (arbno (not #\newline))) skip)
  (identificador(letter (arbno (or letter digit))) symbol)
  (null ("null") string)
  (nombre ("@" letter (arbno letter)) string)
  (numero (digit (arbno digit)) number) 
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (caracter ("'"letter"'") symbol)
  (cadena ("$"(or letter whitespace digit) (arbno (or whitespace letter digit))) string)
  )
)

;###########################################################

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
    
     ;& referencia basada en Java 
    (expresion ("&" identificador) refe-exp)

    ;imp-exp: basado en Java
    (expresion ("print("expresion")") imp-exp)
    
    ;null-exp
    (expresion (null) null-exp)

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
    
    ;Definición de expresiones hexadecimales

    ;Base [8,16,32]
    
    (expresion ("x8(" (arbno expresion)")") hexadecimal-exp)
    (expresion ("x16(" (arbno expresion)")") hexadecimal-exp)
    (expresion ("x32(" (arbno expresion)")") hexadecimal-exp)
    
    ;Estructuras de Control

    ;begin: basado en Java
    (expresion ("begin" expresion ";" (separated-list expresion ";")"end") begin-exp)

    ;if: basado en Java
    (expresion ("if" "(" expresion")" "{" expresion "}" "else" "{" expresion "}") if-exp)

    ;while: basado en Java
    (expresion ("while" "("expresion")" "do" "{"expresion"}" ) while-exp)

    ;for: basado en Java
    (expresion ("for" "{" identificador "=" expresion ";" to expresion "}" "do" expresion) for-exp)
    (to ("to") to-for)
    (to ("downto") down-for)
    
    ;Asignación de Variables
    
    ;set: basado en Java
    (expresion ("set" identificador "=" expresion) set-exp)

    ;Procedimientos
    
    ;let: basado en Java
    (expresion ("let" identificador "=" expresion) let-exp)
  
    ;proc: basado en Java
    (expresion ("proc" "("(separated-list identificador ",") ")" "{" expresion "}") procedure-exp)
    
    ;invocar: basado en Java
    (expresion ("invocar" expresion "(" (separated-list expresion ",") ")") procedure-call-exp)
    
    ;Instancias B-SAT
    (expresion ("FNC" numero "(" (separated-list clausula-or "and") ")") instancia-sat-exp)
    (clausula-or ("("  (separated-list numero "or") ")") clausula-or-exp)
    (expresion ("*"identificador".solve()") solve-instancia-sat-exp)
    
    ;Primitivas aritméticas para enteros

    (primitiva ("+") primitiva-sum)
    (primitiva ("-") primitiva-rest)
    (primitiva ("*") primitiva-mult)
    (primitiva ("%") primitiva-mod)
    (primitiva ("/") primitiva-div)
    (primitiva ("add1") incr-prim)    
    (primitiva ("sub1") decr-prim)
   
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
    
    ;Primitivas sobre vectores
    (primitiva ("vector?") vec-vector?)
    (primitiva ("ref-vector") vec-ref-vector)
    (primitiva ("set-vector") vec-set-vector)
    (primitiva ("crear-vector") vec-crear-vector)
    
     ;Primitivas sobre registros
    (primitiva ("registro?") reg-reg?)
    (primitiva ("ref-registro") reg-ref)
    (primitiva ("set-registro") reg-set)
    (primitiva ("crear-registro") reg-crear)
    
    ;& referencia basada en Java 
    (expresion ("&" nombre) refe-exp)
    
    ;null: basado en Java
    (expresion ("null") null-exp)
    )
    
  
  )
    
;||||||||||||||||||||||||Construcciones Automáticas||||||||||||||||||||||||

(sllgen:make-define-datatypes especificacion-lexica gramatica)
(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica gramatica)
  )
)

;Scan&parse

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica)
)
;Interpretador

(define interpreter
  (sllgen:make-rep-loop "---SMJ>" (lambda (pgm) (eval-program  pgm)) (sllgen:make-stream-parser especificacion-lexica gramatica))
)

;||||||||||||||||||||||||Interpretador||||||||||||||||||||||||

;;Definición (Eval-Program)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-programa (body)
        (eval-expression body (init-env))
      )
    )
  )
)

;;Ambiente Inicial
(define init-env
  (lambda ()
    (extend-env '() '() 
     (empty-env)
    )
  )
)

;#########################Definición del Interprete###############################

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
    
      ;Datos
      (ide-exp (id) (apply-env env id))
      (num-exp (numb) (eval-exp-numeros numb))
      (var-exp (vars vals body) (eval-expresiones-var vars vals body env))                                                     
      (const-exp (vars vals body) (eval-expresiones-cons vars vals body env))
      (cadena-exp (str) (eval-expresiones-cadenas str))
      (caracter-exp (char) (eval-expresiones-caracteres char))
      (primitive-exp (prim list-expres) (eval-expresiones-primitivas prim list-expres env))  
      (refe-exp (ref)
      (cases reference (apply-env-ref env ref)
        (a-ref (pos vals mut) 
               (if (target? (vector-ref vals pos) )
                   (vector-ref vals pos)
                   (indirect-target (apply-env-ref env ref))))))
                   
                   ;Constructores de Datos Predefinidos
      (list-exp (expr-list) (eval-expresiones-listas expr-list env))                                                 
      (vector-exp (expr-vec) (eval-expresiones-vectores expr-vec env))                                             
      (registro-exp (ids exps) (eval-expresiones-registros ids exps env))
      (expr-bool-exp (expres-bol) (eval-expresiones-booleanas expres-bol env))

      ;Estructuras de Control
      (begin-exp (expr exp-lists) (eval-expresiones-begin expr exp-lists env))
      (if-exp (bool-exp true-expr false-expr) (eval-expresiones-if bool-exp true-expr false-expr env))  
      (while-exp (bool-exp body) (eval-expresiones-while bool-exp body env))                                                   
      (for-exp (id init-value goto final-value body) (eval-expresiones-for id init-value goto final-value body env))
      
      ;Procedimientos
      (procedure-exp (ids body) (eval-expresiones-procedure ids body env))
      (procedure-call-exp (expr args) (eval-expresiones-call-procedure expr args env))
      (recursive-exp (proc-names idss bodies letrec-body) (eval-expresiones-recursive proc-names idss bodies letrec-body env))

      ;Asignación de Variables
      (set-exp (id expr) (eval-expresiones-set id expr env))  

      ;Hexadecimales [Base 8,16,32]
      (bas8-exp (hex) (eval-expresiones-hexadecimales hex env))
      ;(bas16-exp (hex) (eval-expresiones-hexa16 hex env))
      ;(bas32-exp (hex) (eval-expresiones-hexa32 hex env))
      
      ;Impresión de Variables                  
      (imp-exp (ex) (display  (eval-expression ex env)))
      
      ;Instancias SAT                                              
      (instancia-sat-exp (first-int clauses)(eval-expresiones-sat first-int clauses ) )                                       
      (solve-instancia-sat-exp (id) (eval-expresiones-solve-sat id env))
      
      )
   )
)

;#######################Definición de Ambientes#######################

;Definición del tipo de ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of variable?))
                       (vec vector?)
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

   ;Definición Ambiente Vacio
(define empty-env  
  (lambda ()
    (empty-env-record)))       

;Extensión del ambiente
(define extend-env
  (lambda (vars vals env)
    (extended-env-record vars (list->vector vals) env)))

;Procedimiento que busca un simbolo en un Ambiente
(define apply-env
  (lambda (env sym)
    (de-ref (apply-env-ref env sym))
  )
)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "Simbolo desconocido" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-simbolo-en-vars sym vars)) (mut (encontrar-valor-mutable sym vars)) )
                             (if (and (number? pos) (symbol? mut) )
                                 (a-ref pos vals mut)
                                 (apply-env-ref env sym)))))))
    

;##############################Scan&Parser##############################

;Definiciones
;(scan&parse "var y=50")
;(scan&parse "const t=null")
;(scan&parse "letrec @x (@s,@d,@k) = q in t")

;Datos
;(scan&parse "5")
;(scan&parse "a")
;(scan&parse "$abc")
;(scan&parse "true")
;(scan&parse "false")

;Constructores de Datos Predefinidos
;(scan&parse "lista (4,2)")
;(scan&parse "vector {4,2,a,b}")
;(scan&parse "registro (a->4; b->6; c->8)")
;(scan&parse "> (5,3)")
;(scan&parse ">= (10,5)")
;(scan&parse "< (5,3)")
;(scan&parse "<= (10,5)")
;(scan&parse "== (5,4)")
;(scan&parse "!= (5,5)")

;Estructuras de Control
;(scan&parse "begin x;s;d end")
;(scan&parse "if( >(5,3)) {5} else {0} ")
;(scan&parse "if( >(5,3)) {true} else {false} ")
;(scan&parse "while (!=(i,0)) do {$holaMundo}")
;(scan&parse "for x=0 to <(x,100) do {2} end")
;(scan&parse "for x=0 to 100 do {if(==(x,100)) {true} else {false} } end")

;Procedimientos

;(scan&parse "let x=1000")
;(scan&parse "proc(a,b,c){true}")
;(scan&parse "invocar x (registro (a->1; b->2))")

;Primitivas para Enteros
;(scan&parse "[+ 10,10]")
;(scan&parse "[- 5,5]")
;(scan&parse "[* 20,5]")
;(scan&parse "[/ 100,10]")
;(scan&parse "[add1 150,151]")
;(scan&parse "[sub1 200,100]")


;Primitivas para Flotantes
;(scan&parse "[+ 5.9,2.8]")
;(scan&parse "[- 1.1,1.0]")
;(scan&parse "[* 500.25,0]")
;(scan&parse "[/ 500.100,250.211]")
;(scan&parse "[add1 200.100,250.211]")
;(scan&parse "[sub1 500.100,250.211]")
;(scan&parse "[+ 5.9,2.8,[- 2,3]]")

;Primitivas para Hexadecimales
;(scan&parse "base8 48 +8 52;")
;(scan&parse "base8 a -8 b;")
;(scan&parse "base8 61 *8 11;")
;(scan&parse "base8 61 ++8 11;")
;(scan&parse "base8 61 --8 11;")
;(scan&parse "base16 52 +16 11;")
;(scan&parse "base16 12 -16 25;")
;(scan&parse "base16 11 *16 25;")
;(scan&parse "base16 27 ++16 42;")
;(scan&parse "base32 100 +32 132;")
;(scan&parse "base32 -200 -32 200;")
;(scan&parse "base32 1000 *32 1000;")

;Primitivas para Cadenas
;(scan&parse "[longitud $hola, $mundo, $4]")
;(scan&parse "[concatenar $flp, 2022, II]")
;(scan&parse "[longitud [concatenar $hola, $mundo, $4]]")

;Primitivas para Listas
;(scan&parse "[vacio? lista(0,1)]")
;(scan&parse "[vacio lista(0,1,2)]")
;(scan&parse "[lista? lista(1,2,3,4,5,f,g)]")
;(scan&parse "[cola (lista (1,2,3,4,5,f,g))]")
;(scan&parse "[cabeza (lista (1,2,3,4,5,f,g))]")
;(scan&parse "[append lista(1,2,3,4,5,f,g)]")

;Primitivas para Vectores
;(scan&parse "[vector? vector{1,a,2,b}]")
;(scan&parse "vector{0,1,2,3,4,5}")
;(scan&parse "[vector? vector{1,2,3}]")
;(scan&parse "[ref-vector vector{1,2,3}]")
;(scan&parse "[set-vector vector{1,2,3}]")

;Primitivas para Registros
;(scan&parse "[registro? registro(a->4; b->$hola)]")
;(scan&parse "registro(a->4; b->$prueba; c->123)")
;(scan&parse "[ref-registro registro(a->$55; b->$100)]")
;(scan&parse "[set-registro registro(a->b; b->a)]")
