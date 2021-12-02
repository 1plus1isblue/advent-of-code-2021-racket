#lang racket

;; Reads a line and adds to the appropriate count
;; String (ForwardCount UpCount DownCount) -> (ForwardCount UpCount DownCount)
(define (parse-line line counts)
  (let* ([words (string-split line)]
         [amt (string->number (second words))]
         [forward-count (first counts)]
         [up-count (second counts)]
         [down-count (third counts)])

    (case (string->symbol (first words))

      ['forward `(,(+ forward-count amt) ,up-count ,down-count)]
      ['down `(,forward-count ,up-count ,(+ down-count amt))]
      ['up `(,forward-count ,(+ up-count amt) ,down-count)]
      )))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define total-counts (foldl parse-line '(0 0 0) lines))
  (define depth (- (third total-counts) (second total-counts)))
  (define horizontal (first total-counts))
  (* horizontal depth)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
