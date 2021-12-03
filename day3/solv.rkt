#lang racket

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define numbers (map (lambda (l) (string->number l 2)) lines))
  (define ones-count (make-hash))
  (hash-set! ones-count 0 0)
  (hash-set! ones-count 1 0)
  (hash-set! ones-count 2 0)
  (hash-set! ones-count 3 0)
  (hash-set! ones-count 4 0)

  (for/list ([number (in-list numbers)])
       (for/list ([bit-index (in-range 0 5)])
            (if (bitwise-bit-set? number bit-index)
              (hash-set! ones-count bit-index (add1 (hash-ref ones-count bit-index)))
              void
              ))
       )

  (define zeros-count (make-hash))

  (for ([bit-index (in-range 0 5)])
       (hash-set! zeros-count bit-index (- (length numbers) (hash-ref ones-count bit-index))))
  (println (format "ones ~a" ones-count))
  (println (format "zeros ~a" zeros-count))

  (define gamma 0)
  (define epsilon 0)
  (for ([bit-index (in-range 0 5)])
       (define val (expt 2 bit-index))
       (if (> (hash-ref zeros-count bit-index)
              (hash-ref ones-count bit-index))
           (begin 
             (println (format "setting epsilon bit ~a to ~a" bit-index val))
						 (set! epsilon (bitwise-ior epsilon val)))
           (begin
             (println (format "setting gamma bit ~a to ~a" bit-index val))
						 (set! gamma (bitwise-ior gamma val)))
         ) 
       )

  (println (format "gamma ~a" gamma))
  (println (format "epsilon ~a" epsilon))
  (println (format "solution: ~a" (* gamma epsilon)))
  )

(with-input-from-file "./test-input.txt"
                      solver
                      #:mode 'text)
