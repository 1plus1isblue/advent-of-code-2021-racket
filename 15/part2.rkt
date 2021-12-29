#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

;; Increments all numbers on a grid by 1 and wraps 10 to 0
;; Vec<Vec<Number> -> Vec<Vec<Number>
(define (increment-grid grid)
  (map-grid (lambda (n) (max 1 (modulo (add1 n) 10))) grid))

(define tests
  (test-suite
    "tests"
    (test-case "increment number by 1 and wrap at 9 back to 1"
               (check-equal? (max 1 (modulo 10 10))
                             1))

    (test-case "increment-entire grid and wrap-around at 9 to 1"
               (define the-grid (vector (vector 1 2 3)
                                        (vector 4 5 6)
                                        (vector 7 8 9)))
               (define expected (vector (vector 2 3 4)
                                        (vector 5 6 7)
                                        (vector 8 9 1)))
               (check-equal? (increment-grid the-grid)
                              expected))
        ;; closes test-suite
    ))

(define unsuccessful-test-count (run-tests tests))

(define (solver) 
  (define risk-grid (map-grid (compose string->number string) (read-grid (current-input-port))))
  (define rows (vector-length risk-grid))
  (define cols (vector-length (vector-ref risk-grid 0)))

  #f)

(if (= unsuccessful-test-count 0)
  (setup-and-run solver)
  (begin
    (println "Tests failed. Skipping solver execution")
    (exit)))
