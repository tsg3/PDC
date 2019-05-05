;;; longitud y longitud_aux
;;; calculan el largo de una lista

(define (longitud lista)
  (longitud_aux 0 lista))

(define (longitud_aux num lista)
  (cond
    ((null? lista)
        num)
    (else
        (longitud_aux (+ num 1) (cdr lista)))
    )
  )

;;; par_orden
;;; calcula el par ordenado de una casilla en relacion al tamaño de la matriz

(define (par_orden size casilla)
  (list (quotient (- casilla 1) size) (modulo (- casilla 1) size)
        )
  )

;;; list_to_par_orden
;;; convierten una lista de casillas a una lista de pares ordenados

(define (list_to_par_orden size lista)
  (cond
    ((null? lista)
        '())
    (else
        (cons (par_orden size (car lista)) (list_to_par_orden size (cdr lista)))
        )
    )
  )

;;; crear_vecinos
;;; busca los vecinos validos de una casilla

(define (vecinos par)
  (vecinos_aux '((1 2) (1 -2) (-1 2) (-1 -2) (2 1) (2 -1) (-2 1) (-2 -1)) par
               )
  )

(define (vecinos_aux movimientos par)
  (cond
    ((null? movimientos)
        '())
    (else
     (cons (list (+ (car par) (caar movimientos)) (+ (cadr par) (cadar movimientos))) (vecinos_aux (cdr movimientos) par))
     )
    )
  )

;;; verificar_movimientos
;;; elimina los vecinos de una casilla que estan fuera de la matriz

(define (verificar_movimientos size movimientos)
  (cond
    ((null? movimientos)
        '())
    ((or (>= (caar movimientos) size) (>= (cadar movimientos) size))
         (verificar_movimientos size (cdr movimientos)))
    ((or (< (caar movimientos) 0) (< (cadar movimientos) 0))
         (verificar_movimientos size (cdr movimientos)))
    (else
        (cons (car movimientos) (verificar_movimientos size (cdr movimientos)))
  )
    )
  )

;;; crear_vecinos
;;; crea los vecinos validos de una casilla

(define (crear_vecinos size casilla)
  (verificar_movimientos size (vecinos casilla))
  )

;;; crear_tablero
;;; crea el grafo que relaciona a las casillas del tablero

(define (crear_tablero size)
  (crear_grafo size (list_to_par_orden size (crear_tablero_aux (* size size) 1)))
  )

(define (crear_tablero_aux size casilla)
  (cond
    ((> casilla size)
        '())
    (else
        (cons casilla (crear_tablero_aux size (+ casilla 1)))
        )
    )
  )

(define (crear_grafo size tablero)
  (cond
    ((null? tablero)
        '())
    (else
        (cons (list (car tablero) (crear_vecinos size (car tablero))) (crear_grafo size (cdr tablero)))
        )
    )
    )

