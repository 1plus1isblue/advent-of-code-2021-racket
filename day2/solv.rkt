#lang racket

;; Reads a line and adds to the appropriate count
;; String (Horizontal Depth) -> (Horizontal Depth)
(define (parse-line line counts)
  (let* ([words (string-split line)]
         [amt (string->number (second words))]
         [horizontal (first counts)]
         [depth (second counts)])

    (case (string->symbol (first words))

      ['forward `(,(+ horizontal amt) ,depth)]
      ['down `(,horizontal ,(+ depth amt))]
      ['up `(,horizontal ,(- depth amt))]
      )))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define total-counts (foldl parse-line '(0 0 0) lines))
  (define depth (second total-counts))
  (define horizontal (first total-counts))
  (* horizontal depth)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
