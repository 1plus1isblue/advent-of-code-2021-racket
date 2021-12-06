#lang racket

(define ROWS 5)
(define COLUMNS 5)
(define the-grid (for/vector ([rows (in-range ROWS)])
                             (make-vector COLUMNS 0)))
;;(define the-grid (vector (vector 0 1 2) (vector 3 4 5) (vector 6 7 8)))

(define (get-cell x y)
  (vector-ref (vector-ref the-grid y) x))

(define (add1-cell x y)
  (vector-set! (vector-ref the-grid y) x (add1 (get-cell x y))))

(define (solver)
  the-grid)

(with-input-from-file "./test-input.txt"
                      solver
                      #:mode 'text)
