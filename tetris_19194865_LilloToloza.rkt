#!r6rs
(import (rnrs lists(6))
        (rnrs base(6))
        (rnrs io simple(6)))

; Laboratorio 1 paradigmas de programación, fecha de entrega 16 de noviembre.

#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · ·|#
;                                       >---- TDA Tablero ----<

; >> Representación
;   Una lista de largo N x M, la cual está llena con ceros o unos, cada valor dentro de la lista
;   representa una posición del tablero, donde 0 significa que esa posición no está ocupada, mientras
;   que 1 indica que la posición si está ocupada.


; >> Constructor

; Función: createBoardRL
; Dom: Entero X Entero X Entero X Entero
; Rec: Board
(define (createBoardRL N M gamePieces seed)

  ; Función: crearListaRL
  ; Dom: Entero
  ; Rec: Lista
  (define (crearListaRL total)
    (if (= total 1)
        (list 0)
        (cons 0 (crearListaRL (- total 1)))
        )
    )
  
  ; Función: faltanPiezas?
  ; Dom: Board X Lista
  ; Rec: Board
  (define (faltanPiezas? board listaRandom) 
    (if (null? (cdr listaRandom))
        board
        (faltanPiezas? (colocarPieza board (cdr (crearPieza (car listaRandom))) (cdr listaRandom)))
        )
    )

  ; Función: colocarPieza
  ; Dom: Board X Pieza
  ; Rec: Board
  (define (colocarPieza board posicionesPieza)
    (if (null? (cdr posicionesPieza))
        board
        (colocarPieza (colocarPosicion board (car posicionesPieza)) (cdr posicionesPieza))
        )
    )

  ; Función: colocarPosicion
  ; Dom: Board X Posicion
  ; Rec: Board
  (define (colocarPosicion board posicion)
    "void"
    )

  
  (cons (cons N M) (crearListaRL (* N M)))
  ;(colocarPiezas 123 (getListaRandom gamePieces seed 5)) ; cuantos actual maximo
  )

; Función: createBoardRC
; Dom: Entero X Entero X Entero X Entero
; Rec: Board
(define (createBoardRC N M gamePieces seed)
  
  ; Función: crearListaRC
  ; Entrada: Entero X Entero
  ; Salida: lista
  (define (crearListaRC N M)
    (define (crearAux contador lista)
      (if (= contador 0)
          lista
          (crearAux (- contador 1) (cons 0 lista))
          )
      )
    (crearAux (* N M) '())
    )
  
  (cons (cons N M) (crearListaRC N M))
  )



; Función: createBoardLazy
; Dom: Entero X Entero X Entero X Entero
; Rec: Board
(define (createBoardLazy N M gamePieces seed)
  "void"
  )


; >> Funciones de pertenencia

(define (chechBoard board)
  "void"
  )

; >> Selectores

; >> Modificadores

(define (play board)
  ("void")
  )

(define (checkHorizontalLines board)
  ("void")
  )

; >> Operadores


#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · |#
;                                       >---- TDA Piezas ----<

; - Representación
;
; Piezas:
;
;   0) ##   1) #    2)  #    3)  ##   4)  #
;      ##      #       ###      ##        #
;              ##                         #
;                                         #
;

; >> Constructor

; Función: crearPieza
; Dom: Número
; Rec: Pieza
(define (crearPieza id)
  (if (and (integer? id) (>= id 0) (<= id 4))
      (cond
        [(= id 0) (list (cons 0 0) (cons 0 0) (cons 0 1) (cons 1 0) (cons 1 1))]
        [(= id 1) (list (cons 1 0) (cons 0 0) (cons 0 1) (cons 1 0) (cons 2 0))]
        [(= id 2) (list (cons 2 0) (cons 0 0) (cons 0 1) (cons 0 2) (cons 1 1))]
        [(= id 3) (list (cons 3 0) (cons 0 0) (cons 0 1) (cons 1 1) (cons 1 2))]
        [(= id 4) (list (cons 4 0) (cons 0 0) (cons 1 0) (cons 2 0) (cons 3 0))]
        )
      (list (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0)) ; Error de entrada
      )
  )


; >> Funciones de pertenencia

; Función: esPieza?
; Dom: Pieza
; Rec: Booleano
(define (esPieza? pieza)
  (and
   (= (length pieza) 5)
   (>= (caar pieza) 0)
   (<= (caar pieza) 4)
   )
  )


; >> Selectores

; Función: quePieza?
; Dom: Pieza
; Rec: Entero
(define (quePieza? pieza)
  (caar pieza)
  )

  
; Función: rotaciones?
; Dom: Pieza
; Rec: Entero
(define (rotaciones? pieza)
  (cdar pieza)
  )

  
; Función: getPosicion
; Dom: Pieza X Entero
; Rec: Par
(define (getPosicion pieza numero)
  (define (getPosAux pieza numero contador)
    (if (= contador numero)
        (car pieza)
        (getPosAux (cdr pieza) numero (+ contador 1))
        )
    )
  (getPosAux pieza numero 0)
  )

; >> Modificadores

; Función: rotarPieza
; Dom: Pieza
; Rec: Pieza
(define (rotarPieza pieza)

  ; Función: rotarPieza1
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza1 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 1 1) (cons 0 0) (cons 0 1) (cons 0 2) (cons 1 2))]
      [(= (rotaciones? pieza) 1) (list (cons 1 2) (cons 0 1) (cons 1 1) (cons 2 0) (cons 2 1))]
      [(= (rotaciones? pieza) 2) (list (cons 1 3) (cons 0 0) (cons 1 0) (cons 1 1) (cons 1 2))]
      [(= (rotaciones? pieza) 3) (crearPieza 1)]
      )
    )

  ; Función: rotarPieza2
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza2 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 2 1) (cons 0 1) (cons 1 0) (cons 1 1) (cons 2 1))]
      [(= (rotaciones? pieza) 1) (list (cons 2 2) (cons 0 1) (cons 1 0) (cons 1 1) (cons 1 2))]
      [(= (rotaciones? pieza) 2) (list (cons 2 3) (cons 0 0) (cons 1 0) (cons 1 1) (cons 2 0))]
      [(= (rotaciones? pieza) 3) (crearPieza 2)]
      )
    )

  ; Función: rotarPieza3
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza3 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 3 1) (cons 0 1) (cons 1 0) (cons 1 1) (cons 2 0))]
      [(= (rotaciones? pieza) 1) (crearPieza 3)]
      )
    )

  ; Función: rotarPieza4
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza4 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 4 1) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4))]
      [(= (rotaciones? pieza) 1) (crearPieza 4)]
      )
    )
  
  (cond
    [(= (quePieza? pieza) 0) pieza]
    [(= (quePieza? pieza) 1) (rotarPieza1 pieza)]
    [(= (quePieza? pieza) 2) (rotarPieza2 pieza)]
    [(= (quePieza? pieza) 3) (rotarPieza3 pieza)]
    [(= (quePieza? pieza) 4) (rotarPieza4 pieza)]
    )
  )


; >> Operadores


#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · |#

; Generador de números aleatorios, obtenido del moodle de la asignatura,
; modificado para que en vez de usar la función remainder, use la función mod.

; Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 127)
; Esta funcion random torma un xn y obtiene el xn+1 de la secuencia de numeros aleatorios.
(define myRandom
  (lambda
      (xn)
    (mod (+ (* a xn) c) m)
    )
  )
; Cada vez que pedimos un random, debemos pasar como argumento el random anterior.


; Aca un ejemplo que permite generar una lista de numeros aleatorios.
; Parametros:
; * "cuantos" indica el largo de la lista a generar.
; * "xActual" valor actual del random, se pasa en cada nivel de recursion de forma actualizada
; * "maximo" Los numeros generados van desde 0 hasta maximo-1
(define getListaRandom
  (lambda (cuantos xActual maximo)
    (if (= 0 cuantos)
        '()
        (let ((xNvo (myRandom xActual)))
          (cons (mod xNvo maximo)
                (getListaRandom (- cuantos 1) xNvo maximo)
                )
          )
        )
    )
  )

(define getRandom
  (lambda (xActual maximo)
    (mod (myRandom xActual) maximo)
    )
  )
