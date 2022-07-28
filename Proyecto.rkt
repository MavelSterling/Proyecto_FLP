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

; Definición Eval-Expression

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      ;Datos
      (ide-exp (id) (apply-env env id))
      (null-exp (null) 'null)
      (num-exp (numb) (implementacion-exp-numeros numb))
      (var-exp (vars vals body) (implementacion-exp-var vars vals body env))                                                     
      (const-exp (vars vals body) (implementacion-exp-cons vars vals body env))
      (cadena-exp (str) (implementacion-exp-cadenas      str))
      (caracter-exp (char) (implementacion-exp-caracteres char))
      (primitive-exp (prim list-expres) (implementacion-exp-primitivas prim list-expres env))  
      (refe-exp (ref)
      (cases reference (apply-env-ref env ref)
        (a-ref (pos vals mut) 
               (if (target? (vector-ref vals pos) )
                   (vector-ref vals pos)
                   (indirect-target (apply-env-ref env ref))))))

      ;Constructores de Datos Predefinidos
      (list-exp (expr-list) (implementacion-exp-listas expr-list env))                                                 
      (vector-exp (expr-vec) (implementacion-exp-vectores expr-vec env))                                             
      (registro-exp (ids exps) (implementacion-exp-registros ids exps env))
      (expr-bool-exp (expres-bol) (implementacion-exp-booleanas expres-bol env))

      ;Estructuras de Control
      (begin-exp (expr exp-lists) (implementacion-exp-begin expr exp-lists env))
      (if-exp (bool-exp true-expr false-expr) (implementacion-exp-if bool-exp true-expr false-expr env))  
      (while-exp (bool-exp body) (implementacion-exp-while bool-exp body env))                                                   
      (for-exp (id init-value goto final-value body) (implementacion-exp-for id init-value goto final-value body env))                 
      
      ;Procedimientos
      (procedure-exp (ids body) (implementacion-exp-procedure         ids body env))
      (procedure-call-exp (expr args) (implementacion-exp-call-procedure        expr args env))
      (recursive-exp (proc-names idss bodies letrec-body) (implementacion-exp-recursivo proc-names idss bodies letrec-body env))

      ;Asignación de Variables
      (set-exp (id expr) (implementacion-exp-set id expr env))
      

      ;Hexadecimales [Base 8,16,32]
      (hexadecimal-exp (hex) (eval-expresiones-hexadecimales hex env))

      ;Impresión de Variables                  
      (imp-exp (ex) (display  (eval-expression ex env)))

      ;Instancias SAT                                              
      (instancia-sat-exp (first-int clauses)(implementacion-exp-sat first-int clauses ) )                                       
      (solve-instancia-sat-exp (id) (implementacion-exp-resolver-sat id env))                                                        
    )
  )
)

;||||||||||||||||||||||||[Paso por valor y paso por referencia]||||||||||||||||||||||||

; Registra una referencia donde pos es la posicion de la referencia en el vector.

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable symbol?)
        )
  )

; Retorna el valor de la referencia del vector.

(define de-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vals mut) 
        (if (target? (vector-ref vals pos))
           (cases target (vector-ref vals pos)
              (indirect-target (refi) (primitive-deref refi))
           )
          (primitive-deref ref)
        )
      )
    )       
  )
)

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mut)
             (vector-ref vec pos)
      )
    )
  )
)


;Cambia el valor de la referencia por el valor dado

(define setref!
  (lambda (ref val)
    (if (target? ref)
        (cases target ref
          (indirect-target (refi) (primitive-setref! refi val))
        )
        (primitive-setref! ref val)
    )
  )
)

(define primitive-setref!
     (lambda (ref val)
       (cases reference ref
         (a-ref (pos vec mut) (vector-set! vec pos val))
       )
     )
    )

; Definición de Targets

(define-datatype target target?
  (indirect-target (ref  ref-to-direct-target? ))

)

(define aplicar-indirect-target
  (lambda (val)
    (cases target val (indirect-target (ref)  (primitive-deref ref)))
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec mut)
                  (if  (not (reference?  (vector-ref vec pos) ) )  #t #f )
                  )
           )
         )
    )
  )
; Definición de una variable cuando muta y cuando no.

(define-datatype variable variable?
  (mutable (id symbol?))
  (inmutable (id symbol?))
)
; Definición de una función para encontrar el simbolo en una lista de variables.

(define encontrar-sim-var
  (lambda (sym vars)
    (busqueda-symb-in-vars sym vars 0)
  )
)
; Definición de función auxiliar de busqueda simbolo en una lista de variables.

(define busqueda-symb-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) pos )
      ( else (busqueda-symb-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)
; Definición de función para encontrar en el ambiente la variable que tenga el mismo simbolo y retornarla.

(define var-es-mutable?
  (lambda (sym env)
      (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No se encontró ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-sim-var sym vars)))
                             (if (number? pos)
                                 (cases variable (list-ref vars pos)
                                   (mutable (id) #t)
                                   (inmutable (id) #f)
                                 )
                                 (var-es-mutable? sym env)
                             )
        )
      )
  )
 )
)
; Definición que retorna cuando una variable es mutable o no.

(define encontrar-valor-mutable
 (lambda (sym vars)
   (busqueda-mut-in-vars sym vars 0)
 )
)

; Definición de función auxiliar que actúa como búsqueda en la lista de variables.

(define busqueda-mut-in-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (retornar-simbolo (car vars))) (cases variable (car vars) (mutable (id) 'M) (inmutable (id) 'I) )  )
      ( else (busqueda-mut-in-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)

; Definición que retorna el símbolo de la variable.

(define retornar-simbolo
  (lambda (var)
    (cases variable var
      (mutable (id) id)
      (inmutable (id) id)
    )
  )
)

  ;||||||||||||||||||||||||[Definición de Ambientes]||||||||||||||||||||||||

;Definición del tipo de ambiente.

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of variable?))
                       (vec vector?)
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;Definición ambiente vacío.

(define empty-env  
  (lambda ()
    (empty-env-record)))       

;Extensión del ambiente.

(define extend-env
  (lambda (vars vals env)
    (extended-env-record vars (list->vector vals) env)))

;Procedimiento que busca un simbolo en un ambiente.

(define apply-env
  (lambda (env sym)
    (de-ref (apply-env-ref env sym))
  )
)

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "Simbolo desconocido ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-sim-var sym vars)) (mut (encontrar-valor-mutable sym vars)) )
                             (if (and (number? pos) (symbol? mut) )
                                 (a-ref pos vals mut)
                                 (apply-env-ref env sym)
                                 )
                             )
                           )
      )
    )
  )
     

; Definición que retorna una lista de los números desde 0 hasta end

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

; Definición de tipos de datos nativos.

; Definición de true-valor como un valor de verdad.

(define true-value 'true)

; Definición de false-valor como un valor de falsedad.

(define false-value 'false)

;Definición que pregunta cuando algo es verdadero.

(define isTrue?
  (lambda (x)
    (equal? x true-value)
  )
)
; Definición de datatype para datos de tipo instancia SAT.

(define-datatype sat sat?
  (instancia-sat (n integer?) (lista list?))
)
; Definición de datatype que define el tipo de dato árbol binario SAT.

(define-datatype arbol-sat arbol-sat?
  (nodo (value number?) (arbol-izquierda arbol-sat?) (arbol-derecha arbol-sat?))
  (arbol-vacio)
)

;||||||||||||||||||||||||[Desarrollo de Implementaciones]||||||||||||||||||||||||

; Implementación para evaluar expresiones númericas.

(define implementacion-exp-numeros
  (lambda (numb)
    (cases num numb
     (entero->numero (numb) numb)
     (float->numero (numb) numb)
    )
  )
)

; Implementación para evaluar expresiones booleanas.

(define implementacion-exp-booleanas
  (lambda (expr env)
    (cases  expr-bool expr
      (pred-prim-bool (pred first-expr second-expr)
                      (cases pred-prim pred
                        (menor-bool() (if (< (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (mayor-bool() (if (> (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (mayor-igual-bool() (if (>= (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (menor-igual-bool() (if (<= (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (igual-bool() (if (equal? (eval-expression first-expr env) (eval-expression second-expr env)) true-value false-value))
                        (diferente-bool() (if (not (equal? (eval-expression first-expr env) (eval-expression second-expr env))) true-value false-value))
                        )
                      )
      (oper-bin (pred first-expr second-expr)
                (cases oper-bin-bool pred
                  (and-boolean-primitive() (if (and (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value))
                  (or-boolean-primitive() (if (or (isTrue? (eval-expression first-expr env)) (isTrue? (eval-expression second-expr env))) true-value false-value)) 
                  )
                )
      (oper-un (unary-prim  bool-exp)
               (cases oper-un-bool unary-prim
                 (not-boolean-primitive() (if (isTrue? (eval-expression bool-exp env)) false-value true-value))
                 )
               )
      (bool-expr-bool (bool)
                      (cases  boolean bool 
                        (true->boolean() true-value)
                        (false->boolean() false-value)
                        )
                      )
      )
    )
    
    
    ; Implementación para evaluar procedimientos de tipo procval.

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)
   )
  )

; Función que evalua el cuerpo de un procedimientos en el ambiente extendido.

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env  (definir-mutabilidad ids args) args env)
                                )))))

; Función que define la mutabilidad de los argumentos envíados.

(define definir-mutabilidad
  (lambda (ids args)
   (cond
     ((null? ids) empty)
     (else (if (target? (car args))
               (cases target (car args)
                 (indirect-target (ref) (cases reference ref (a-ref (pos vec mut)
           (if (equal? mut 'M)
                (cons (mutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args)))
                (cons (inmutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args)))
                )))))
                (cons (mutable (car ids)) (definir-mutabilidad (cdr ids) (cdr args)))
       )
     )
   )
  )
)

; Implementación de tipo procedure.

(define implementacion-exp-procedure        
  (lambda (ids body env)
    (closure ids body env)
  )
)
; Implementación de tipo call-procedure.

(define implementacion-exp-call-procedure       
  (lambda (expr args env)
    (let (
        (proc (eval-expression expr env))
          (argumentos  (implementacion-exp-listas args env))
         )  
         (if (procval? proc)
                     (apply-procedure proc argumentos)
                     (eopl:error 'eval-expression
                                 "No se puede aplicar el procedimiento para ~s" proc))
      )
  )
)
)

; Implementación de cadenas.

(define implementacion-exp-cadenas     
  (lambda (str)
    (substring str 1 (- (string-length str) 1))
  )
)

; Implementación de caracteres.

(define implementacion-exp-caracteres
  (lambda (str)
    (string->symbol (substring str 1 (- (string-length str) 1)))
  )
)


; Implementación de operaciones hexadecimales.

(define eval-expresiones-hexadecimales
  (lambda (expr env)
     (implementacion-exp-listas expr env)
  )
)
(define suma-base
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma-base (predecessor x) y)))))

(define resta-base
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta-base  x (predecessor y))))))

(define multiplicacion-base
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma-base (multiplicacion-base (predecessor x) y) y))
    ))

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
