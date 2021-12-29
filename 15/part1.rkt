#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

;; Gets minimal total cost for a path to (x,y) based
;; on the risks grid
(define lookup (make-hash))
(define (min-total-cost risks x y)
  (if (hash-has-key? lookup `(,x ,y))
    (hash-ref lookup `(,x ,y))
    (begin 
      (hash-set! lookup
                 `(,x ,y)
                 (let ([xy-risk (grid-ref x y risks)])
                   (cond [(and (= x 0) (= y 0)) (grid-ref x y risks)]
                         [(= x 0) (+ xy-risk (min-total-cost risks x (sub1 y)))] ; left-wall
                         [(= y 0) (+ xy-risk (min-total-cost risks (sub1 x) y))] ; top-wall
                         [else (+ xy-risk (min (min-total-cost risks (sub1 x) y)
                                               (min-total-cost risks x (sub1 y))))])))
      (hash-ref lookup `(,x ,y)))))

(define tests
  (test-suite
    "tests"
        ;; closes test-suite
    ))

(define unsuccessful-test-count (run-tests tests))

(define (solver)
  (define risk-grid (map-grid (compose string->number string) (read-grid (current-input-port))))
  (grid-set! 0 0 risk-grid 0)
  (define rows (vector-length risk-grid))
  (define cols (vector-length (vector-ref risk-grid 0)))
  (define stored-risks (curry min-total-cost risk-grid))
  (println (time (stored-risks (sub1 cols) (sub1 rows))))
  #f)

(if (= unsuccessful-test-count 0)
  (setup-and-run solver)
  (begin
    (println "Tests failed. Skipping solver execution")
    (exit)))
