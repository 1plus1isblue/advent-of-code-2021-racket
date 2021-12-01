#lang racket
(require "helpers.rkt")

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
  (count-increases lines))

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)



