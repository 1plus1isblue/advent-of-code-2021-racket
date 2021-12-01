#lang racket

;; List<Pair<Integer, Integer>> -> Number
(define (fn l1 l2 acc)
  (cond [(empty? l1) acc]
        [(> (first l1) (first l2)) (fn (rest l1) (rest l2) (add1 acc))]
        [(<= (first l1) (first l2)) (fn (rest l1) (rest l2) acc)]
        [else (raise (format "uh oh: ~a, ~a, acc"))]))

;; the number of times a depth measurement increases
;; 199 (N/A - no previous measurement)
;; 200 (increased)
;; 208 (increased)
;; 210 (increased)
;; 200 (decreased)
;; 207 (increased)
;; 240 (increased)
;; 269 (increased)
;; 260 (decreased)
;; 263 (increased)
(define (solver)
  (define lines (map string->number (port->lines (current-input-port))))
  (fn lines (cons 0 lines) -1))

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)




