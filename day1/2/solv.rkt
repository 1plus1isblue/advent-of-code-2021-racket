#lang racket

(define (count-increases leaving entering count)
  (cond
    [(null? entering) count]
    [(> (first entering) (first leaving)) (count-increases (rest leaving) (rest entering) (add1 count))]
    [else  (count-increases (rest leaving) (rest entering) count)]))

(define (solver)
  (define lines (map string->number (port->lines (current-input-port))))
  (count-increases lines (list-tail lines 3) 0)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
