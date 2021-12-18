#lang racket

(define (unique-number? n)
  (or (= n 2)
      (= n 3)
      (= n 4)
      (= n 7)))

(define (take-output line)
  (car (cdr (string-split line "|"))))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define output-lines (map take-output lines))
  (define flattened-output (flatten (map string-split output-lines)))
  (count unique-number? (map string-length flattened-output))
  )

(define filename (vector-ref (current-command-line-arguments) 0))

(with-input-from-file filename 
                      solver
                      #:mode 'text)
