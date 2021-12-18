#lang racket

(define (solver)
  (define crabs (map string->number (string-split (read-line (current-input-port)) ",")))
  (define min-crab (apply min crabs))
  (define max-crab (apply max crabs))
  (define targets (for/list ([target (in-range min-crab (add1 max-crab))])
       (for/sum ([crab (in-list crabs)])
                (abs (- target crab))
                )
       ))
  (apply min targets)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
