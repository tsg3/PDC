#lang racket/base
(require graphics/graphics)


  ;((draw-rectangle w) (make-posn (* i 100) 0) 100 100))

(define (PDC-paint numMatriz solucion)
  (cond ((< numMatriz 5)
        (display "Solucion no encontrada para el valor numerico de la matriz"))
        (else
   (pintar numMatriz solucion))))

(define (pintar numMatriz solucion)
  (open-graphics)
  
; nothing appears to happen, but the library is initialized...
 
  (define w (open-viewport "practice" (* numMatriz 100) (* numMatriz 100)))
; viewport window appears
 
  (for ([i (in-range numMatriz)])
    (for ([j (in-range numMatriz)])
      ((draw-rectangle w) (make-posn (* i 100) (* j 100)) 100 100)))

  
  (animar numMatriz solucion solucion w)
  ;((draw-solid-rectangle w) (make-posn (+ (* (cadr (car solucion)) 100) 1) (+ (* (car(car solucion)) 100) 1)) 98 98 (make-rgb 1 0 1))
  )
(define (animar numMatriz solucion solucionAbsoluta viewport)
  (cond ((null? solucion)
         (display "Ya termino la animacion"))
        (else
         ((draw-solid-rectangle viewport) (make-posn (+ (* (car (car solucion)) 100) 1) (+ (* (cadr (car solucion)) 100) 1)) 98 98 (make-rgb 1 0 1))
         (track (car solucion) solucionAbsoluta viewport)
         (sleep 1)
         (animar numMatriz (cdr solucion) solucionAbsoluta viewport))))
          
  
(define (track parOrden solucion viewport)
  (cond ((equal? parOrden (car solucion))
         #t)
        (else
         ((draw-line viewport) (make-posn (+ (* (car (car solucion)) 100) 50) (+ (* (cadr (car solucion)) 100) 50))
                               (make-posn (+ (* (car (cadr solucion)) 100) 50) (+ (* (cadr (cadr solucion)) 100) 50))
                               (make-rgb 0 0 0))
         (track parOrden (cdr solucion) viewport))))
         