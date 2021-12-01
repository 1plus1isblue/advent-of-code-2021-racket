#lang racket
(require "../part1/helpers.rkt")

;; # Approach to solution
;; 1. create triplets of list 
;; 2. for each triplet create sum
;; 3. apply same method from part1


;; List -> Boolean
(define (not-null? a-list)
  (andmap (lambda (element) (not (empty? element))) a-list))

;; Creates a list of triplets for consecutive values of a list.
;; The first 2 values contain nulls, e.g. <number, nil, nil> and <number, number, nil>
;; List<Number> -> List<Triplet<Number, Number, Number>>
(define (build-triplets l1)
  (define l2 (cons null l1))
  (define l3 (cons null l2))
  (define triplets (filter not-null? (zip l1 l2 l3)))
  (define sums (map (lambda (triplet) (apply + triplet)) triplets))
  (count-increases sums))

;; Zips 3 lists together
;; List<Number> List<Number> List<Number -> List<Triplet<Number, Number, Number>>
(define (zip l1 l2 l3)
  (if (empty? l1)
    empty
    (cons `(,(first l1) ,(first l2) ,(first l3))
          (zip (rest l1) (rest l2) (rest l3)))))

(define (solver)
  (define lines (map string->number (port->lines (current-input-port))))
  (define triplets (build-triplets lines))
  triplets
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
