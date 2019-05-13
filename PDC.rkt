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

(define (num size casilla)
  (+ (* size (car casilla)) (cadr casilla) 1)
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

(define (list_to_num size lista)
  (cond
    ((null? lista)
        '())
    (else
        (cons (num size (car lista)) (list_to_num size (cdr lista)))
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

(define (crear_tablero size casilla una_solucion)
  (buscar_ruta casilla (* size size) (crear_grafo size (list_to_par_orden size (crear_tablero_aux (* size size) 1))) una_solucion)
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

;;; g
;;; grafo de ejemplo (5x5)

(define g '(((0 0) ((1 2) (2 1)))
 ((0 1) ((1 3) (2 2) (2 0)))
 ((0 2) ((1 4) (1 0) (2 3) (2 1)))
 ((0 3) ((1 1) (2 4) (2 2)))
 ((0 4) ((1 2) (2 3)))
 ((1 0) ((2 2) (0 2) (3 1)))
 ((1 1) ((2 3) (0 3) (3 2) (3 0)))
 ((1 2) ((2 4) (2 0) (0 4) (0 0) (3 3) (3 1)))
 ((1 3) ((2 1) (0 1) (3 4) (3 2)))
 ((1 4) ((2 2) (0 2) (3 3)))
 ((2 0) ((3 2) (1 2) (4 1) (0 1)))
 ((2 1) ((3 3) (1 3) (4 2) (4 0) (0 2) (0 0)))
 ((2 2) ((3 4) (3 0) (1 4) (1 0) (4 3) (4 1) (0 3) (0 1)))
 ((2 3) ((3 1) (1 1) (4 4) (4 2) (0 4) (0 2)))
 ((2 4) ((3 2) (1 2) (4 3) (0 3)))
 ((3 0) ((4 2) (2 2) (1 1)))
 ((3 1) ((4 3) (2 3) (1 2) (1 0)))
 ((3 2) ((4 4) (4 0) (2 4) (2 0) (1 3) (1 1)))
 ((3 3) ((4 1) (2 1) (1 4) (1 2)))
 ((3 4) ((4 2) (2 2) (1 3)))
 ((4 0) ((3 2) (2 1)))
 ((4 1) ((3 3) (2 2) (2 0)))
 ((4 2) ((3 4) (3 0) (2 3) (2 1)))
 ((4 3) ((3 1) (2 4) (2 2)))
 ((4 4) ((3 2) (2 3)))))

;;; eliminar
;;; eliminar elemento de lista

(define (eliminar ele list)
  (cond
    ( (equal? ele (car list)) (cdr list) )
    ( (null? (cdr list)) list)
    ( else (cons (car list) (eliminar ele (cdr list))) )
    )
  )

;;; append-element
;;; inserta elemento al final de lista

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

;;; miembro
;;; verifica que un elemento exista en una lista

(define (miembro ele list)
  (cond ( (null? list) '() )
        ( (equal? (car list) ele ) ele)
        (else (miembro ele (cdr list)) )
    )
  )

;;; obtener_vecinos
;;; busca los vecinos de una casilla

(define (obtener_vecinos ele graph)
  (cond ( (null? graph) '() )
        ( (equal? (caar graph) ele ) (cadar graph))
        (else (obtener_vecinos ele (cdr graph)) )
    )
  )

;;; buscar_ruta, buscar_ruta_aux, buscar_nueva_ruta, buscar_nueva_ruta_aux, verificar_ruta
;;; realiza el objetivo de PDC-Sol y PDC-Todas

(define (buscar_ruta u limite grafo una_solucion)
  (buscar_ruta_aux 1 u u limite grafo una_solucion '())
  )

(define (buscar_ruta_aux n ruta nodo limite grafo una_solucion rutas)
  (cond
    ((< n limite)
        (buscar_nueva_ruta n ruta nodo limite (obtener_vecinos nodo grafo) grafo una_solucion rutas))
    (else
        (list_to_num (sqrt limite) ruta))
    )
  )

(define (buscar_nueva_ruta n ruta nodo limite vecinos grafo una_solucion rutas)
  (cond
    (una_solucion 
       (buscar_nueva_ruta_aux n ruta nodo limite (quicksort_vecinos grafo vecinos) grafo una_solucion rutas))
    (else
       (buscar_nueva_ruta_aux n ruta nodo limite vecinos grafo una_solucion rutas))
    )
  )

(define (buscar_nueva_ruta_aux n ruta nodo limite vecinos grafo una_solucion rutas)
  (cond
    ((null? vecinos)
        #f)
    ((null? (miembro (car vecinos) ruta))
      (cond
        (una_solucion 
            (verificar_ruta n ruta nodo limite vecinos (buscar_ruta_aux (+ n 1) (append-element ruta (car vecinos)) (car vecinos) limite grafo una_solucion rutas) grafo una_solucion rutas))
        (else
            (unir (listar(buscar_ruta_aux (+ n 1) (append-element ruta (car vecinos)) (car vecinos) limite grafo una_solucion rutas)) (listar(buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion rutas))))))
    (else
        (buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion rutas))
    )
  )

(define (verificar_ruta n ruta nodo limite vecinos ruta2 grafo una_solucion rutas)
  (cond
    ((not ruta2)
        (buscar_nueva_ruta_aux n ruta nodo limite (cdr vecinos) grafo una_solucion rutas))
    (else
        ruta2)
    )
  )

;;; listar
;;; convierte un #f en '() si es necesario

(define (listar lst)
  (cond
    ((equal? lst #f)
        '())
    (else
        lst)
    )
  )

;;; unir
;;; une listas (vacias, ruta, conjunto de rutas) para PDC-Todas

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

;;; last-element
;;; saca elementos de la lista para retroceder en el arbol de busqueda de soluciones del grafo

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

;;; PDC-Sol

(define (PDC-Sol size casilla)
  (cond
    ((verificar_casilla size casilla)
        (crear_tablero size casilla #t))
    (else
        #f)
    )
  )

;;; PDC-Todas

(define (PDC-Todas size casilla)
  (cond
    ((verificar_casilla size casilla)
        (crear_tablero size casilla #f))
    (else
        #f)
    )
  )

;;; verificar_casilla
;;; verifica si un vecino existe dentro de la matriz/tablero

(define (verificar_casilla size casilla)
  (cond
    ((and (>= (car casilla) 0) (< (car casilla) size) (>= (cadr casilla) 0) (< (cadr casilla) size))
        #t)
    (else
        #f)
    )
  )

;;; quicksort_vecinos
;;; ordena la lista de vecinos de un nodo de el que tenga menor vecinos al que tenga mas (PDC-Sol)

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

