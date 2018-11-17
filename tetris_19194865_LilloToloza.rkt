#lang racket

; Laboratorio 1 paradigmas de programación, fecha de entrega 16 de noviembre.

#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · ·|#
;                                       >---- TDA Tablero ----<

; >> Representación
;   Tablero:
;      Lista de elementos, '(5 10 (1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))
;   Donde los dos primeros números corresponden al ancho y al alto respectivamente, el tercero a un cotenedor y el cuarto al puntaje.
;
;   Contenedor:
;      Una lista de largo N x M, la cual está llena con ceros o unos, cada valor dentro de la lista
;   representa una posición del tablero, donde 0 significa que esa posición no está ocupada, mientras
;   que 1 indica que la posición si está ocupada.



; >> Constructor <<

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

  ; Función: quedanPiezas?
  ; Dom: Lista X Lista
  ; Rec: Lista
  (define (quedanPiezas? listaTablero listaAleatorios)
    (if (null? listaAleatorios)
        listaTablero
        (quedanPiezas? (colocarPieza listaTablero (crearPieza (car listaAleatorios)) (getRandom seed 5)) (cdr listaAleatorios))
        )
   )

  ; Función: colocarPiezas
  ; Dom: Lista X Pieza
  ; Rec: Lista
  (define (colocarPieza listaTablero pieza horizontal)
    "NULL"
    )

  (list N M (crearListaRL (* N M)) 0)
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
  
  (list N M (crearListaRC N M) 0)
  )

; Función: createBoardLazy
; Dom: Entero X Entero X Entero X Entero
; Rec: Board
(define (createBoardLazy N M gamePieces seed)
  "No implementada aun"
  )



; >> Funciones de pertenencia <<

(define (checkBoard board)
  (if (= (length board) 4)
      (if (and (and (integer? (car board)) (>= (car board) 5))
               (and (integer? (cadr board)) (>= (cadr board) 10))
               (list? (caddr board))
               (integer? (cadddr board)))
          #t
          #f
          )
      #f
      )
  )



; >> Selectores <<

; Función: getDimensiones
; Dom: Board
; Rec: Par
(define (getDimensiones board)
  (cons (car board) (cadr board))
  )

; Función: getContenedor
; Dom: Board
; Rec: lista
(define (getContenedor board)
  (caddr board)
  )

; Función: getPuntaje
; Dom: Board
; Rec: Entero
(define (getPuntaje board)
  (cadddr board)
  )

; Función: carRepetido
; Dom: Lista X Entero X Lista
; Rec: Lista
(define (carRepetido lista veces listaAux)
  (if (= veces 0)
    (reverse listaAux)
    (carRepetido (cdr lista) (- veces 1) (cons (car lista) listaAux))
    )
)
        
; Función: cdrRepetido
; Dom: Lista
; Rec: Lista
(define (cdrRepetido lista veces)
  (if (= veces 0)
      lista
      (cdrRepetido (cdr lista) (- veces 1))
      )
  )


; Función: getLineaHorizontal
; Dom: Lista X Entero X Entero
; Rec: Lista
(define (getLineaHorizontal contenedor ancho numLinea)
  (if (= numLinea 0)
      (carRepetido contenedor ancho '())
      (getLineaHorizontal (cdrRepetido contenedor ancho) ancho (- numLinea 1))
      )
  )




; >> Modificadores <<

; Función: play
; Dom: Board X Entero X Pieza
; Rec: Board
(define (play board posHoriz piece)
  "No implementada aun"
  )



; >> Operadores <<

; Función: nextPiece
; Dom: Board X Entero
; Rec: Pieza
(define (nextPiece board seed)
  (define (siguienteAux semilla maximo)
    (crearPieza (getRandom semilla maximo))
    )
  (siguienteAux seed 5)
  )

; Función: checkHorizontalLines
; Dom: Board
; Rec: Lista
(define (checkHorizontalLines board)

  ; Función: checkHorAux
  ; Dom: Lista X Entero
  ; Rec: String
  (define (checkHorAux contenedor contador)
    (if (= contador (cdr (getDimensiones board)))
        "No hay lineas horizontales llenas"
        (if (todosIguales? (getLineaHorizontal contenedor (car (getDimensiones board)) contador))
            (if (= (car (getLineaHorizontal contenedor (car (getDimensiones board)) contador)) 1)
                (string-append "La linea horizontal número " (number->string (+ contador 1)) " está llena")
                (checkHorAux contenedor (+ contador 1))
                )
            (checkHorAux contenedor (+ contador 1))
            )
        )
    )

  ; Función: todosIguales?
  ; Dom: Lista
  ; Rec: Booleano
  (define (todosIguales? lista)
    (or
     (or (null? lista) (null? (cdr lista)))
     (and (eq? (car lista) (cadr lista)) (todosIguales? (cdr lista)))
     )
    )

  (checkHorAux (getContenedor board) 0)
  )
  
; Función: board->string
; Dom: Board
; Rec: String
(define (board->string board)

  ; Función: bStringAux
  ; Dom: Lista X Entero X String
  ; Rec: String
  (define (bStringAux contenedor ancho alto contador string)
    (if (= contador alto)
        string
        (bStringAux contenedor ancho alto (+ contador 1) (string-append (juntarStringRecursivo (reverse (getLineaHorizontal contenedor ancho contador)) ancho "") string))
        )
  )

  ; Función: juntarStringRecursivo
  ; Dom: Lista X Entero
  ; Rec: String
  (define (juntarStringRecursivo lista contador stringSalida)
    (if (= contador 0)
        stringSalida
        (if (= contador (car (getDimensiones board)))
            (juntarStringRecursivo (cdr lista) (- contador 1) (string-append (number->string (car lista)) "\n"))
            (juntarStringRecursivo (cdr lista) (- contador 1) (string-append (number->string (car lista)) stringSalida))
            )
        )
    )
  (bStringAux (getContenedor board) (car (getDimensiones board)) (cdr (getDimensiones board)) 0 "")
  )




#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · |#
;                                       >---- TDA Piezas ----<

; - Representación
;      Lista de pares '((1 . 0) (0 . 0) (0 . 1) (1 . 0) (2 . 0))
;   Donde el primer par representa el ID de la pieza y la cantidad de rotaciones.
;   Los demás pares indican cada posición de cada bloque que compone la pieza.
;
; Piezas:
;
;   0) ##   1) #    2)  #    3)  ##   4)  #
;      ##      #       ###      ##        #
;              ##                         #
;                                         #
;



; >> Constructor <<

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

; >> Funciones de pertenencia <<

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



; >> Selectores <<

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



; >> Modificadores <<

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



; >> Operadores <<


#| · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · · |#

; Generador de números aleatorios, obtenido del moodle de la asignatura,
; por temas de uso de listas el programa está en lang racket y funciona remainder,
; para el caso contrario de usar #!r6rs se puede cambiar por la funcion mod.

; Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 127)
; Esta funcion random torma un xn y obtiene el xn+1 de la secuencia de numeros aleatorios.
(define myRandom
  (lambda
      (xn)
    (remainder (+ (* a xn) c) m)
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
          (cons (remainder xNvo maximo)
                (getListaRandom (- cuantos 1) xNvo maximo)
                )
          )
        )
    )
  )

(define getRandom
  (lambda (xActual maximo)
    (remainder (myRandom xActual) maximo)
    )
  )


; _____________________________________________________________________________________________

#| EJEMPLOS DE PRUEBA PARA REVISION

FUNCIÓN createBoardRL:
 - (createBoardRL 5 10 0 0)
 - (createBoardRL 7 16 11 2018)
 - (createBoardRL 4 12 4 12)

FUNCIÓN createBoardRC:
 - (createBoardRC 5 10 0 0)
 - (createBoardRC 7 16 11 2018)
 - (createBoardRC 4 12 4 12)

FUNCIÓN createBoardLazy: No implementada

FUNCIÓN checkBoard:
 - (checkBoard '(5 10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))
 - (checkBoard '(7 16 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))
 - (checkBoard '(4 12 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))

FUNCIÓN nextPiece:
 - (nextPiece '(5 10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0) 2018)
 - (nextPiece '(6 11 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0) 7)
 - (nextPiece '(7 12 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0) 23)

FUNCIÓN play: No implementada

FUNCIÓN checkHorizontalLines:
 - (checkHorizontalLines '(5 10 (0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))
 - (checkHorizontalLines '(6 11 (0 1 1 0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 0 1 0 1 1 1 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1) 0))
 - (checkHorizontalLines '(5 10 (1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0) 0))

FUNCIÓN board->string
 - (board->string '(5 10 (0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0))
 - (board->string '(6 11 (0 1 1 0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 0 1 0 1 1 1 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1) 0))
 - (board->string '(5 10 (1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0) 0))
 
|#







