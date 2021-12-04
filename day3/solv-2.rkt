#lang racket

(define MAX-BIT-INDEX 11)

(define (numbers_10->numbers_2 list-of-numbers)
  (map (lambda (n) (number->string n 2)) list-of-numbers))

(define (pred-builder bit-index)
  (lambda (n) (bitwise-bit-set? n bit-index)))

(define (rating-builder first-pred second-pred)
  (lambda (rating list-of-numbers bit-index)
    (cond 
      [(= (length list-of-numbers) 1) (first list-of-numbers)]
      [else 
        (define-values (ones zeros) (partition (pred-builder bit-index) list-of-numbers))
        (define new-index (sub1 bit-index))
        (cond
          [(first-pred (length ones) (length zeros)) (rating ones new-index)]
          [(second-pred (length ones) (length zeros)) (rating zeros new-index)]
          )])))

(define (oxygen-rating list-of-numbers bit-index)
  (cond 
    [(= (length list-of-numbers) 1) (first list-of-numbers)]
    [else 
      (define-values (ones zeros) (partition (pred-builder bit-index) list-of-numbers))
      (define new-index (sub1 bit-index))
      (cond
        [(>= (length ones) (length zeros)) (oxygen-rating ones new-index)]
        [(< (length ones) (length zeros)) (oxygen-rating zeros new-index)]
        )]))

(define (co2-rating list-of-numbers bit-index)
  (cond 
    [(= (length list-of-numbers) 1) (first list-of-numbers)]
    [else 
      (define-values (ones zeros) (partition (pred-builder bit-index) list-of-numbers))
      (define new-index (sub1 bit-index))
      (cond
        [(< (length ones) (length zeros)) (co2-rating ones new-index)]
        [(>= (length ones) (length zeros)) (co2-rating zeros new-index)]
        )]))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define numbers (map (lambda (l) (string->number l 2)) lines))
  (define oxygen (oxygen-rating numbers MAX-BIT-INDEX))
  (define co2 (co2-rating numbers MAX-BIT-INDEX))
  (* oxygen co2))

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
