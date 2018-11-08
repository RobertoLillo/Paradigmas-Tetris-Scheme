#!r6rs
(import (rnrs lists(6))
        (rnrs base(6))
        (rnrs io simple(6)))

; Laboratorio 1 paradigmas de programación, fecha de entrega 16 de noviembre.

#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · ·|#
;                                       >---- TDA Tablero ----<

; >> Representación
;   Una lista de largo N x M, la cual está llena con ceros o unos, 0 implica que esa posición no
;   está ocupada, mientras que 1 indica que la posición si está ocupada.


; >> Constructor

; Función: createBoardRL
; Dom: Entero X Entero X Entero X Entero
; Rec: Board
(define (createBoardRL N M gamePieces seed)

  ; Función: crearListaRL
  ; Dom: Entero X Entero
  ; Rec: Lista
  (define (crearListaRL total)
    (if (= total 1)
        (list 0)
        (cons 0 (crearListaRL (- total 1)))
        )
    )
  
  (crearListaRL (* N M))
  )


; Función: createBoardRC
; Dom: entero, entero, entero, entero
; Rec: Lista
(define (createBoardRC N M gamePieces seed)
  
  ; Función: crearListaRC
  ; Entrada: entero, entero
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
  
  (crearListaRC N M)
  )



; Función: createBoardLazy
; Entrada: entero, entero, entero, entero
; Salida: lista
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
;   1) ##   2) #    3)  #    4)  ##   5)  #
;      ##      #       ###      ##        #
;              ##                         #
;                                         #
;

; >> Constructor

; Función: crearPieza
; Dom: Número
; Rec: Pieza
(define (crearPieza id)
  (if (and (integer? id) (> id 0) (< id 6))
      (cond
        [(= id 1) (list (cons 1 0) (cons 0 0) (cons 0 1) (cons 1 0) (cons 1 1))]
        [(= id 2) (list (cons 2 0) (cons 0 0) (cons 0 1) (cons 1 0) (cons 2 0))]
        [(= id 3) (list (cons 3 0) (cons 0 0) (cons 0 1) (cons 0 2) (cons 1 1))]
        [(= id 4) (list (cons 4 0) (cons 0 0) (cons 0 1) (cons 1 1) (cons 1 2))]
        [(= id 5) (list (cons 5 0) (cons 0 0) (cons 1 0) (cons 2 0) (cons 3 0))]
        )
      (list (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0))
      )
  )


; >> Funciones de pertenencia

; Función: esPieza?
; Dom: Pieza
; Rec: Booleano
(define (esPieza? pieza)
  (and
   (= (length pieza) 5)
   (> (caar pieza) 0)
   (< (caar pieza) 5)
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

  ; Función: rotarPieza2
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza2 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 2 1) (cons 0 0) (cons 0 1) (cons 0 2) (cons 1 2))]
      [(= (rotaciones? pieza) 1) (list (cons 2 2) (cons 0 1) (cons 1 1) (cons 2 0) (cons 2 1))]
      [(= (rotaciones? pieza) 2) (list (cons 2 3) (cons 0 0) (cons 1 0) (cons 1 1) (cons 1 2))]
      [(= (rotaciones? pieza) 3) (crearPieza 2)]
      )
    )

  ; Función: rotarPieza3
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza3 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 3 1) (cons 0 1) (cons 1 0) (cons 1 1) (cons 2 1))]
      [(= (rotaciones? pieza) 1) (list (cons 3 2) (cons 0 1) (cons 1 0) (cons 1 1) (cons 1 2))]
      [(= (rotaciones? pieza) 2) (list (cons 3 3) (cons 0 0) (cons 1 0) (cons 1 1) (cons 2 0))]
      [(= (rotaciones? pieza) 3) (crearPieza 3)]
      )
    )

  ; Función: rotarPieza4
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza4 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 4 1) (cons 0 1) (cons 1 0) (cons 1 1) (cons 2 0))]
      [(= (rotaciones? pieza) 1) (crearPieza 4)]
      )
    )

  ; Función: rotarPieza5
  ; Dom: Pieza
  ; Rec: Pieza
  (define (rotarPieza5 pieza)
    (cond
      [(= (rotaciones? pieza) 0) (list (cons 5 1) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4))]
      [(= (rotaciones? pieza) 1) (crearPieza 5)]
      )
    )
  
  (cond
    [(= (quePieza? pieza) 1) pieza]
    [(= (quePieza? pieza) 2) (rotarPieza2 pieza)]
    [(= (quePieza? pieza) 3) (rotarPieza3 pieza)]
    [(= (quePieza? pieza) 4) (rotarPieza4 pieza)]
    [(= (quePieza? pieza) 5) (rotarPieza5 pieza)]
    )
  )


; >> Operadores

