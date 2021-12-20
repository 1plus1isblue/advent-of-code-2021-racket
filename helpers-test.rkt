#lang racket
(require "helpers.rkt"
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
    "tests"

    (test-case "memoize-2 function returns a function that takes 2 arguments"
               (define call-count 0)
               (define memoized-fn (memoize-2 (lambda (x y) (begin
                                                        (set! call-count (add1 call-count))
                                                        (+ x y)))))
               (memoized-fn 1 2)
               (check-equal? call-count 1)
               (memoized-fn 2 3)
               (check-equal? call-count 2)
               (memoized-fn 1 2)
               (check-equal? call-count 2))
               

        ;; closes test-suite
    ))
(run-tests tests)
