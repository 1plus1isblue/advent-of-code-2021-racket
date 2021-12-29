#lang racket
(require "helpers.rkt"
         rackunit
         rackunit/text-ui)

(define tests
  (test-suite
    "tests"
    (test-case "memoize-1"
               (define (fibonacci n)
                 (cond ((= n 0) 0)
                       ((= n 1) 1)
                       (else (+ (fibonacci (- n 2))
                                (fibonacci (- n 1))))))

               (set! fibonacci (memoize-1 fibonacci))
               (check-equal? (time (fibonacci 40))
                             102334155)
               )

    ;; closes test-suite
    ))
(run-tests tests)
