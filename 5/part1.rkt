#lang racket

(define ROWS 999)
(define COLUMNS 999)
(define the-grid (for/vector ([rows (in-range ROWS)])
                             (make-vector COLUMNS 0)))
;;(define the-grid (vector (vector 0 1 2) (vector 3 4 5) (vector 6 7 8)))

(define (get-cell x y)
  (vector-ref (vector-ref the-grid y) x))

(define (add1-cell x y)
  (vector-set! (vector-ref the-grid y)
               x
               (add1 (get-cell x y))))

(define (flatten-vector vector-of-vectors)
  (flatten (vector->list (vector-map vector->list vector-of-vectors))))

(define (recursive-list-map fn a-list)
  (cond [(null? a-list) null]
        [(list? (car a-list))
         (cons (recursive-list-map fn (car a-list))
               (recursive-list-map fn (cdr a-list)))]
        [(not (list? (car a-list)))
         (cons (fn (car a-list)) (recursive-list-map fn (cdr a-list)))]))

(define (horizontal? segment)
  (define y1 (get-y (get-start segment)))
  (define y2 (get-y (get-end segment)))
  (define result (= y1 y2))
  result
  )

(define (vertical? segment)
  (define x1 (get-x (get-start segment)))
  (define x2 (get-x (get-end segment)))
  (define result (= x1 x2))
  result
  )

(define (get-start segment)
  (car segment))

(define (get-end segment)
  (car (cdr segment)))

(define (get-x point)
  (car point))

(define (get-y point)
  (car (cdr point)))

(define (add1-horizontal segment)
  (define sorted-segment (sort segment (lambda (p1 p2) (< (get-x p1) (get-x p2)))))
  (let ([x1 (get-x (get-start sorted-segment))]
        [y1 (get-y (get-start sorted-segment))]
        [x2 (get-x (get-end sorted-segment))])
    (for ([x-curr (in-range x1 (add1 x2))])
         (add1-cell x-curr y1))))

(define (add1-vertical segment)
  (define sorted-segment (sort segment (lambda (p1 p2) (< (get-y p1) (get-y p2)))))
  (let ([x1 (get-x (get-start sorted-segment))]
        [y1 (get-y (get-start sorted-segment))]
        [y2 (get-y (get-end sorted-segment))])
    (for ([y-curr (in-range y1 (add1 y2))])
         (add1-cell x1 y-curr))
    ))

(define (add1-segment segment)
  (cond [(horizontal? segment) (add1-horizontal segment)]
        [(vertical? segment) (add1-vertical segment)]
        [else void])) ;; handle diagonal later

;; example: "9,4 -> 3,4 ""
(define (add-line-to-grid line)
  (define split-line (map string-trim (string-split line " -> "))) 
  (define start-end-string-pair (map (lambda (l) (string-split l ",")) split-line))
  (define segment (recursive-list-map string->number start-end-string-pair))

  (add1-segment segment)
)

(define (print-grid)
  (for ([row (in-vector the-grid)])
       (println row)))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (for ([line (in-list lines)])
       (add-line-to-grid line))
  (count (lambda (n) (>= n 2)) (flatten-vector the-grid))
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
