(require errortrace) ;; Ofrece más información al producirse un error
(require racket/trace) ;; Permite seguir la ejecución de una función usando antes (trace nombre_de_función)

(define (fibo n)
    (cond 
        ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (fibo (- n 1)) (fibo (- n 2))))
    )
)

;;(trace fibo)
;;(display (fibo 7))

(define (expo b n)
    (cond
        ((= b 0) 0)
        ((= n 0) 1)
        ((> n 0) (* b (expo b (- n 1))))
    )
)

;;(trace expo)
;;(display (expo 2 3))

(define (minimo lista)
    (cond
        ((null? (cdr lista)) (car lista))
        ((<(car lista) (car(cdr lista))) (minimo(cons(car lista)(cdr(cdr lista)))))
        ((>= (car lista) (car(cdr lista))) (minimo (cdr lista)))
    )
)

;;(trace minimo)
;;(display (minimo '(7 6 3 2 9 1)))

(define (inserta num lista)
    (cond
        ((null? lista) (cons num '()))
        ((< num (car lista))(cons num lista))
        ((null? (cdr lista)) (cons (car lista) (inserta num '())))
        ((and(>= num (car lista)) (<= num (car(cdr lista))))(cons (car lista)(cons num (cdr lista))))
        ((or(< num (car lista)) (> num (car(cdr lista))))(cons (car lista)(inserta num (cdr lista))))
    )
)

;;(trace inserta)
;;(display (inserta 55 '(5 6 30 60 90)))

(define (concatena  L1 L2)
    (cond
        ((null? L1) L2)
        (else (cons (car L1) (concatena (cdr L1) L2)))
    )
)

;;(trace concatena)
;;(display (concatena '(a b c) '(a (b c) d e)))

(define (invierte lista) 
    (if (null? lista)
        '()
        (concatena (invierte (cdr lista)) (list (car lista)))
    )
)

;;(trace invierte)
;;(display (invierte '(a (b c) d (e f))))

(define (elimina elemento lista)
    (if (null? lista) 
        '()
        (if (equal? (car lista) elemento)
            (elimina elemento (cdr lista))
            (cons (car lista) (elimina elemento (cdr lista)))
        )
    )
)

;;(trace elimina)
;;(display (elimina 'a '(f e d a h a d a)))

(define (buscar elemento lista)
    (if (null? lista)
        #f
        (if (equal? (car lista) elemento)
            #t
            (buscar elemento (cdr lista))
        )
    )
)

(define (repetidos lista)
    (if (null? lista)
        '()
        (if (buscar (car lista) (cdr lista))
            (repetidos (cdr lista))
            (cons (car lista) (repetidos (cdr lista)))
        )
    )
)

;;(trace repetidos)
;;(trace buscar)
;;(display (repetidos '(a b c d a f g d c c h)))
    



