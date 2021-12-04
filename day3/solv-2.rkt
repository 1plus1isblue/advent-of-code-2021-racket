#lang racket

(define MAX-BIT-INDEX 11)

(define (pred-builder bit-index)
  (lambda (n) (bitwise-bit-set? n bit-index)))

(define (rating-builder first-pred second-pred)
  (define (rating list-of-numbers bit-index)
    (cond 
      [(= (length list-of-numbers) 1) (car list-of-numbers)]
      [else 
        (define-values (ones zeros) (partition (pred-builder bit-index) list-of-numbers))
        (define new-index (sub1 bit-index))
        (cond
          [(first-pred (length ones) (length zeros)) (rating ones new-index)]
          [(second-pred (length ones) (length zeros)) (rating zeros new-index)]
          )]))
  rating)

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define numbers (map (lambda (l) (string->number l 2)) lines))
  (define oxygen ((rating-builder >= <) numbers MAX-BIT-INDEX))
  (define co2 ((rating-builder < >=) numbers MAX-BIT-INDEX))
  (* oxygen co2))

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
