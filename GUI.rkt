#lang racket/base
(require graphics/graphics)
(provide PDC-paint)

;;; Funcion: PDC-paint
;;; Entradas: numMatriz: tamano de la matriz
;;;           solucion: solucion del problema del caballo
;;; Salida: Interfaz de la animacion del caballo
(define (PDC-paint numMatriz solucion)
  (cond ((or (< numMatriz 5) (not (is_valida numMatriz solucion)))
        (display "Solucion no encontrada para el valor numerico de la matriz"))
        (else
   (pintar numMatriz(list_to_par_orden numMatriz solucion)))))

(define (pintar numMatriz solucion)

  (open-graphics)
  
  (define w (open-viewport "PDC-Paint" (* numMatriz 100) (* numMatriz 100)))
 
  (for ([i (in-range numMatriz)])
    (for ([j (in-range numMatriz)])
      ((draw-rectangle w) (make-posn (* i 100) (* j 100)) 100 100)))

  (animar numMatriz solucion solucion w)
  )

;;; Funcion: animar
;;; Entradas: numMatriz: Tamano de la matriz
;;;           solucion: solucion del problema del caballo que se ira recortando hasta se nula
;;;           solucionAbsoluta: solucion COMPLETA del problema del caballo
;;;           viewport: Ventana donde se hara la animacion
;;; Salida: Animacion del problema del caballo
(define (animar numMatriz solucion solucionAbsoluta viewport)
  (cond ((null? solucion)
         (display "Ya termino la animacion"))
        (else
         (cuadrado (car solucion) solucionAbsoluta viewport)
         (track (car solucion) solucionAbsoluta viewport)
         (caballo (car solucion) viewport)
         (sleep 1)
         (animar numMatriz (cdr solucion) solucionAbsoluta viewport))))
          
;;; Funcion: track
;;; Entradas: parOrden: Par ordenado referente a por cual par va de la solucion y da la condicion de parada
;;;           solucion: solucion COMPLETA del problema del caballo
;;;           viewport: Ventana donde se dara el rastro
;;; Salida: Rastro que deja el caballo al moverse  
(define (track parOrden solucion viewport)
  (cond ((equal? parOrden (car solucion))
         #t)
        (else
         ((draw-line viewport) (make-posn (+ (* (cadr (car solucion)) 100) 50) (+ (* (car (car solucion)) 100) 50))
                               (make-posn (+ (* (cadr (cadr solucion)) 100) 50) (+ (* (car (cadr solucion)) 100) 50))
                               (make-rgb 0 0 0))
         (track parOrden (cdr solucion) viewport))))

;;; Funcion: caballo
;;; Entradas: parOrden: Par ordenado donde se pinta el caballo
;;;           viewport: Ventana donde se pintara el caballo
;;; Salida: Pinta el caballo en el par ordenado acordado  
(define (caballo parOrden viewport)
  (cond ((null? parOrden)
         #t)
        (else
         ((draw-pixmap viewport) "C:\\Users\\PT\\Desktop\\PDC\\PDC.png" (make-posn(+ (* (cadr parOrden) 100) 1)(+ (* (car parOrden) 100) 1))))))

;;; Funcion: cuadrado
;;; Entradas: parOrden: Par ordenado referente a por cual par va de la solucion y da la condicion de parada
;;;           solucion: solucion COMPLETA del problema del caballo
;;;           viewport: Ventana donde se pintara el rastro
;;; Salida: Pinta un cuadrado en el par ordenado acordado  
(define (cuadrado parOrden solucion viewport)
  (cond ((equal? parOrden (car solucion))
         #t)
        (else
         ((draw-solid-rectangle viewport) (make-posn (+ (* (cadr (car solucion)) 100) 1) (+ (* (car (car solucion)) 100) 1)) 98 98 (make-rgb 1 0 1))
         (cuadrado parOrden (cdr solucion) viewport))))

;;; Funcion: par_orden
;;; Entradas: size : tamaño de la matriz que se esté usando
;;;           casilla : numero de la casilla
;;; Salida: Convierte un numero de casilla a par ordenado

(define (par_orden size casilla)
  (list (quotient (- casilla 1) size) (modulo (- casilla 1) size)
        )
  )

;;; Funcion: list_to_par_orden
;;; Entradas: size : tamaño de la matriz que se esté usando 
;;;           lista : lista de numeros
;;; Salida: Convierte una lista de numeros a una lista de pares ordenados

(define (list_to_par_orden size lista)
  (cond
    ((null? lista)
        '())
    (else
        (cons (par_orden size (car lista)) (list_to_par_orden size (cdr lista)))
        )
    )
  )

;;; is_valida
;;; Entradas: size: tamano de la matriz solucion
;;; 					solucion: solucion en una lista del probelma del caballo
;;; Salida: #t si la sulucion es valida y #f si no

(define (is_valida size solucion)
  (cond
    ((and (= (longitud solucion) (* size size)) (is_valida_aux solucion '()) (is_valida_aux2 size (list_to_par_orden size solucion)))
     #t)
    (else
     #f)
   )
  )

;;; is_valida_aux
;;; Entradas: solucion a verificar y lista vacia
;;; Salida: #t si no se repiten elementos en la solucion
;;;         #f si se repiten elementos en la solucion

(define (is_valida_aux solucion list)
  (cond
    ((null? solucion)
     #t)
    ((member (car solucion) list)
     #f)
    (else
     (is_valida_aux (cdr solucion) (cons (car solucion) list) ))
    )
  )

;;; is_valida_aux2
;;; Entradas: tamano del tablero y la solucion a verificar
;;; Salida: #t si los movimientos de la solcuion son de caballo y no se salen del tablero
;;;         #f si no cumple con lo dicho

(define (is_valida_aux2 size solucion)
  (cond
    ((= (longitud solucion) 2)
     (cond
           ((and (< (+ (caar solucion) 1) 5) (< (+ (cadar solucion) 2) 5) (= (+ (caar solucion) 1) (caadr solucion)) (= (+ (cadar solucion) 2) (cadadr solucion))) #t)
           ((and (< (+ (caar solucion) 1) 5) (> (+ (cadar solucion) -2) -1) (= (+ (caar solucion) 1) (caadr solucion)) (= (+ (cadar solucion) -2) (cadadr solucion))) #t)
           ((and (> (+ (caar solucion) -1) -1) (< (+ (cadar solucion) 2) 5) (= (+ (caar solucion) -1) (caadr solucion)) (= (+ (cadar solucion) 2) (cadadr solucion))) #t)
           ((and (> (+ (caar solucion) -1) -1) (> (+ (cadar solucion) -2) -1) (= (+ (caar solucion) -1) (caadr solucion)) (= (+ (cadar solucion) -2) (cadadr solucion))) #t)
           ((and (< (+ (caar solucion) 2) 5) (< (+ (cadar solucion) 1) 5) (= (+ (caar solucion) 2) (caadr solucion)) (= (+ (cadar solucion) 1) (cadadr solucion))) #t)
           ((and (< (+ (caar solucion) 2) 5) (> (+ (cadar solucion) -1) -1) (= (+ (caar solucion) 2) (caadr solucion)) (= (+ (cadar solucion) -1) (cadadr solucion))) #t)
           ((and (> (+ (caar solucion) -2) -1) (< (+ (cadar solucion) 1) 5) (= (+ (caar solucion) -2) (caadr solucion)) (= (+ (cadar solucion) 1) (cadadr solucion))) #t)
           ((and (> (+ (caar solucion) -2) -1) (> (+ (cadar solucion) -1) -1) (= (+ (caar solucion) -2) (caadr solucion)) (= (+ (cadar solucion) -1) (cadadr solucion))) #t)
           (else #f)
       )
     )
    ((and (< (+ (caar solucion) 1) 5) (< (+ (cadar solucion) 2) 5) (= (+ (caar solucion) 1) (caadr solucion)) (= (+ (cadar solucion) 2) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (< (+ (caar solucion) 1) 5) (> (+ (cadar solucion) -2) -1) (= (+ (caar solucion) 1) (caadr solucion)) (= (+ (cadar solucion) -2) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (> (+ (caar solucion) -1) -1) (< (+ (cadar solucion) 2) 5) (= (+ (caar solucion) -1) (caadr solucion)) (= (+ (cadar solucion) 2) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (> (+ (caar solucion) -1) -1) (> (+ (cadar solucion) -2) -1) (= (+ (caar solucion) -1) (caadr solucion)) (= (+ (cadar solucion) -2) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (< (+ (caar solucion) 2) 5) (< (+ (cadar solucion) 1) 5) (= (+ (caar solucion) 2) (caadr solucion)) (= (+ (cadar solucion) 1) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (< (+ (caar solucion) 2) 5) (> (+ (cadar solucion) -1) -1) (= (+ (caar solucion) 2) (caadr solucion)) (= (+ (cadar solucion) -1) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (> (+ (caar solucion) -2) -1) (< (+ (cadar solucion) 1) 5) (= (+ (caar solucion) -2) (caadr solucion)) (= (+ (cadar solucion) 1) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    ((and (> (+ (caar solucion) -2) -1) (> (+ (cadar solucion) -1) -1) (= (+ (caar solucion) -2) (caadr solucion)) (= (+ (cadar solucion) -1) (cadadr solucion)) )
     (is_valida_aux2 size (cdr solucion)))
    
    (else
     #f)
   )
  )

;;; Funcion: longitud
;;; Entradas: lista : lista de elementos 
;;; Salida: Longitud de la lista

(define (longitud lista)
  (longitud_aux 0 lista))

;;; Funcion: longitud_aux
;;; Entradas: lista : lista de elementos 
;;;           num : cantidad de elementos contados
;;; Salida: Longitud de la lista

(define (longitud_aux num lista)
  (cond
    ((null? lista)
        num)
    (else
        (longitud_aux (+ num 1) (cdr lista)))
    )
  )