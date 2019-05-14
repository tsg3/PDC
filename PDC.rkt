#lang racket
(require "GUI.rkt")

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

;;; Funcion: par_orden
;;; Entradas: size : tamaño de la matriz que se esté usando
;;;           casilla : numero de la casilla
;;; Salida: Convierte un numero de casilla a par ordenado

(define (par_orden size casilla)
  (list (quotient (- casilla 1) size) (modulo (- casilla 1) size)
        )
  )

;;; Funcion: num
;;; Entradas: size : tamaño de la matriz que se esté usando 
;;;           casilla : par ordenado de la casilla
;;; Salida: Convierte un par ordenado a un numero de casilla

(define (num size casilla)
  (+ (* size (car casilla)) (cadr casilla) 1)
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

;;; Funcion: list_to_num
;;; Entradas: size : tamaño de la matriz que se esté usando 
;;;           lista : lista de pares ordenados
;;; Salida: Convierte una lista de pares ordenados a una lista de numeros de casilla

(define (list_to_num size lista)
  (cond
    ((null? lista)
        '())
    (else
        (cons (num size (car lista)) (list_to_num size (cdr lista)))
        )
    )
  )

;;; Funcion: vecinos
;;; Entradas: par : par ordenado
;;; Salida: Lista con los vecinos de una casilla con respecto al movimiento del caballo

(define (vecinos par)
  (vecinos_aux '((1 2) (1 -2) (-1 2) (-1 -2) (2 1) (2 -1) (-2 1) (-2 -1)) par
               )
  )

;;; Funcion: vecinos
;;; Entradas: movimientos : movimientos de un caballo de ajedrez
;;;           par : par ordenado
;;; Salida: Lista con los movimientos posibles de una casilla

(define (vecinos_aux movimientos par)
  (cond
    ((null? movimientos)
        '())
    (else
     (cons (list (+ (car par) (caar movimientos)) (+ (cadr par) (cadar movimientos))) (vecinos_aux (cdr movimientos) par))
     )
    )
  )

;;; Funcion: verificar_movimientos
;;; Entradas: size : Tamaño de la matriz que se está usando
;;;           movimientos : vecinos de una casilla por el movimiento del caballo
;;; Salida: Elimina los vecinos de una casilla que se encuentren fuera del tablero

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

;;; Funcion: verificar_movimientos
;;; Entradas: size : Tamaño de la matriz que se está usando
;;;           casilla : par ordenado de la casilla
;;; Salida: Lista con los vecinos válidos de una casilla

(define (crear_vecinos size casilla)
  (verificar_movimientos size (vecinos casilla))
  )

;;; Funcion: crear_tablero
;;; Entradas: size : Tamaño de la matriz que se está usando
;;;           casilla : par ordenado de la casilla
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Ruta o rutas encontradas como solucion para la casilla

(define (crear_tablero size casilla una_solucion)
  (buscar_ruta casilla (* size size) (crear_grafo size (list_to_par_orden size (crear_tablero_aux (* size size) 1))) una_solucion)
  )

;;; Funcion: crear_tablero_aux
;;; Entradas: size : Tamaño de la matriz que se está usando
;;;           casilla : par ordenado de la casilla
;;; Salida: Lista con todas las casillas para un tablero de tamaño size.

(define (crear_tablero_aux size casilla)
  (cond
    ((> casilla size)
        '())
    (else
        (cons casilla (crear_tablero_aux size (+ casilla 1)))
        )
    )
  )

;;; Funcion: crear_grafo
;;; Entradas: size : Tamaño de la matriz que se está usando
;;;           tablero : lista con las casillas en pares ordenados de un tablero
;;; Salida: Grafo que representa las interconexiones entre todas las casillas del tablero

(define (crear_grafo size tablero)
  (cond
    ((null? tablero)
        '())
    (else
        (cons (list (car tablero) (crear_vecinos size (car tablero))) (crear_grafo size (cdr tablero)))
        )
    )
    )

;;; Funcion: eliminar
;;; Entradas: ele : elemento
;;;           list : lista de elementos
;;; Salida: Retorna la lista list sin el elemento ele

(define (eliminar ele list)
  (cond
    ( (equal? ele (car list)) (cdr list) )
    ( (null? (cdr list)) list)
    ( else (cons (car list) (eliminar ele (cdr list))) )
    )
  )

;;; Funcion: element
;;; Entradas: lst : lista
;;;           elem : elemento
;;; Salida: Concatena al final de la lista lst, el elemento elem

(define (append-element lst elem)
  (cond
    ((null? lst)
        elem)
    ((list? (car lst))
        (append lst (list elem)))
    (else
        (append (list lst) (list elem)))
    )
  )

;;; Funcion: element
;;; Entradas: ele : lista
;;;           list : elemento
;;; Salida: Verifica si ele es miembro de la lista list

(define (miembro ele list)
  (cond ( (null? list) '() )
        ( (equal? (car list) ele ) ele)
        (else (miembro ele (cdr list)) )
    )
  )

;;; Funcion: obtener_vecinos
;;; Entradas: ele : casilla del tablero
;;;           graph : grafo
;;; Salida: Busca los vecinos de ele en el grafo graph

(define (obtener_vecinos ele graph)
  (cond ( (null? graph) '() )
        ( (equal? (caar graph) ele ) (cadar graph))
        (else (obtener_vecinos ele (cdr graph)) )
    )
  )

;;; Funcion: buscar_ruta
;;; Entradas: u : casilla del tablero
;;;           limite : cantidad de casillas del tablero
;;;           grafo : grafo correspondiente al tablero
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Retorna la ruta/las rutas como solución al problema

(define (buscar_ruta u limite grafo una_solucion)
  (buscar_ruta_aux 1 u u limite grafo una_solucion)
  )

;;; Funcion: buscar_ruta_aux
;;; Entradas: n : cantidad de casillas visitadas
;;;           ruta : camino recorrido hasta la casilla actual
;;;           nodo : casilla del tablero
;;;           limite : cantidad de casillas del tablero
;;;           grafo : grafo correspondiente al tablero
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Busca los vecinos del nodo o retorna la ruta si n = limite

(define (buscar_ruta_aux n ruta nodo limite grafo una_solucion)
  (cond
    ((< n limite)
        (buscar_nueva_ruta n ruta nodo limite (obtener_vecinos nodo grafo) grafo una_solucion))
    (else
        (list_to_num (sqrt limite) ruta))
    )
  )

;;; Funcion: buscar_nueva_ruta
;;; Entradas: n : cantidad de casillas visitadas
;;;           ruta : camino recorrido hasta la casilla actual
;;;           nodo : casilla del tablero
;;;           limite : cantidad de casillas del tablero
;;;           vecinos : vecinos de nodo
;;;           grafo : grafo correspondiente al tablero
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Busca si es posible encontrar una ruta en los vecinos 

(define (buscar_nueva_ruta n ruta nodo limite vecinos grafo una_solucion)
  (cond
    (una_solucion 
       (buscar_nueva_ruta_aux n ruta nodo limite (quicksort_vecinos grafo vecinos) grafo una_solucion))
    (else
       (buscar_nueva_ruta_aux n ruta nodo limite vecinos grafo una_solucion))
    )
  )

;;; Funcion: buscar_nueva_ruta_aux
;;; Entradas: n : cantidad de casillas visitadas
;;;           ruta : camino recorrido hasta la casilla actual
;;;           nodo : casilla del tablero
;;;           limite : cantidad de casillas del tablero
;;;           vecinos : vecinos de nodo
;;;           grafo : grafo correspondiente al tablero
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Busca rutas en los vecinos del nodo. Retorna #f si no encontró una ruta.

(define (buscar_nueva_ruta_aux n ruta nodo limite vecinos grafo una_solucion)
  (cond
    ((null? vecinos)
        #f)
    ((null? (miembro (car vecinos) ruta))
      (cond
        (una_solucion 
            (verificar_ruta n ruta nodo limite vecinos (buscar_ruta_aux (+ n 1) (append-element ruta (car vecinos)) (car vecinos) limite grafo una_solucion) grafo una_solucion))
        (else
            (unir (listar(buscar_ruta_aux (+ n 1) (append-element ruta (car vecinos)) (car vecinos) limite grafo una_solucion)) (listar(buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion))))))
    (else
        (buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion))
    )
  )

;;; Funcion: verificar_ruta
;;; Entradas: n : cantidad de casillas visitadas
;;;           ruta : camino recorrido hasta la casilla actual
;;;           nodo : casilla del tablero
;;;           limite : cantidad de casillas del tablero
;;;           vecinos : vecinos de nodo
;;;           ruta2 : ruta encontrada ( '() si no encontró ninguna)
;;;           grafo : grafo correspondiente al tablero
;;;           una_solucion : bandera que determina si se buscan una (#t) o todas las soluciones (#f)
;;; Salida: Busca en los otros vecinos si no encontró una ruta (backtracking para PDC-Sol)

(define (verificar_ruta n ruta nodo limite vecinos ruta2 grafo una_solucion)
  (cond
    ((not ruta2)
        (buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion))
    (else
        ruta2)
    )
  )

;;; Funcion: listar
;;; Entradas: lst : lista de elementos o #f
;;; Salida: Convierte lst en una lista vacía si su valor es #f

(define (listar lst)
  (cond
    ((equal? lst #f)
        '())
    (else
        lst)
    )
  )

;;; Funcion: unir
;;; Entradas: lst1, lst2 : listas de rutas
;;; Salida: concatena listas

(define (unir lst1 lst2)
  (cond
    ((null? lst1)
        lst2)
    ((null? lst2)
        lst1)
    ((and (null? lst1) (null? lst2))
        '())
    ((and (not (list? (car lst1))) (not (list? (car lst2))))
        (list lst1 lst2))
    ((not (list? (car lst1)))
        (cons lst1 lst2))
    ((not (list? (car lst2)))
        (append lst1 (list lst2)))
    (else
        (append lst1 lst2))
    )
  )

;;; Funcion: last-element
;;; Entradas: lst : lista de elementos
;;; Salida: Obtiene el ultimo elemento de una lista

(define (last-element list)
  (cond
    ((null? (cdr list))
        '())
    ((null? (cddr list))
        (car list))
    (else
        (last-element (cdr list)))
    )
  )

;;; Funcion: PDC-Sol
;;; Entradas: size : tamaño de la matriz que se quiere usar
;;;           casilla : casilla del tablero
;;; Salida: Busca una solucion para el problema del caballo en la casilla

(define (PDC-Sol size casilla)
  (cond
    ((verificar_casilla size casilla)
        (crear_tablero size casilla #t))
    (else
        #f)
    )
  )

;;; Funcion: PDC-Sol
;;; Entradas: size : tamaño de la matriz que se quiere usar
;;;           casilla : casilla del tablero
;;; Salida: Busca todas las soluciones para el problema del caballo en la casilla

(define (PDC-Todas size casilla)
  (cond
    ((verificar_casilla size casilla)
        (crear_tablero size casilla #f))
    (else
        #f)
    )
  )

;;; Funcion: verificar_casilla
;;; Entradas: size : tamaño de la matriz que se quiere usar
;;;           casilla : casilla del tablero
;;; Salida: Verifica si la casilla pertenece a un tablero de tamaño 'size' * 'size'

(define (verificar_casilla size casilla)
  (cond
    ((and (>= (car casilla) 0) (< (car casilla) size) (>= (cadr casilla) 0) (< (cadr casilla) size))
        #t)
    (else
        #f)
    )
  )

;;; Funcion: quicksort_vecinos, quicksort_vecinos_aux
;;; Entradas: grafo : grafo que se está usando 
;;;           list : lista de elementosa ordenar
;;;           lista : lista de vecinos para comparar
;;;           pivote : elemento que se usa para comparar los demás elementos
;;;           menores : lista con vecinos con menor cantidad de vecinos que el pivote
;;;           mayores : lista con vecinos con mayor cantidad de vecinos que el pivote
;;; Salida: Ordena una lista de vecinos del que tenga menor cantidad de vecinos a mayor cantidad

(define (quicksort_vecinos grafo list)
  (cond
    ((null? list) list)
    (else (quicksort_vecinos_aux grafo (cdr list) (car list) '() '()))
))


(define (quicksort_vecinos_aux grafo lista pivote menores mayores)
  (cond
    ((null? lista)
            (append (quicksort_vecinos grafo menores) (cons pivote (quicksort_vecinos grafo mayores))))
    ((< (longitud (obtener_vecinos (car lista) grafo)) (longitud(obtener_vecinos pivote grafo)))
            (quicksort_vecinos_aux grafo (cdr lista) pivote (cons (car lista) menores) mayores) )
    (else
            (quicksort_vecinos_aux grafo (cdr lista) pivote menores (cons (car lista) mayores) )
)
  ))

;;; PDC-Test
;;; Entradas: size: tamano de la matriz solucion
;;; 					solucion: solucion en una lista del probelma del caballo
;;; Salida: la matriz solucion si existe y #f si no

(define (PDC-Test size solucion)
  (cond
    ((is_valida size solucion) (sol_to_matrix size solucion))
     (else #f)
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

;;; sol_to_matrix
;;; Entradas: tamano del tablero y la solucion ya verificada
;;; Salida: la matriz de la sulucion

(define (sol_to_matrix size solucion)
  (display_matrix 1  (* size size) (sol_to_matrix_aux 1 (* size size) solucion))
  )

;;; sol_to_matrix_aux
;;; Entradas: un contador el num de casillas del tablero y la solucion
;;; Salida: la traducion de la matriz pero en una sola linea

(define (sol_to_matrix_aux contador size solucion)
  (cond
   ((> contador size) '())
   (else (cons (busca_num contador 1 solucion) (sol_to_matrix_aux (+ contador 1) size solucion) ))
   )
  )

;;; busca_num
;;; Entradas: dos contadores que buscan la posicision de los movimientos y la solucion
;;; Salida: la posicion del movimiento en el tablero

(define (busca_num contador contador2 solucion)
  (cond
    ((= contador (car solucion)) contador2)
    (else (busca_num contador (+ contador2 1) (cdr solucion)))
    )
  )

;;; display_matrix
;;; Entradas: contador, num de casillas en la matriz y la matriz como lista
;;; Salida: la matriz en el display

(define (display_matrix num size matrix)
  (cond
    ((= size 0) (display " "))
    ((= num 1) (display "| ") (display (car matrix)) (display " | ") (display_matrix (+ num 1) (- size 1) (cdr matrix)))
    ((> num 5) (display "\n") (display_matrix 1 size matrix))
    (else (display (car matrix)) (display " | ") (display_matrix (+ num 1) (- size 1) (cdr matrix)))
    )
  )
