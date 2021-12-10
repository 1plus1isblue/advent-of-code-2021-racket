#lang racket

(define OPENING (set #\( #\[ #\{ #\<))
(define CLOSING (set #\) #\] #\} #\>))
(define PAIRING (make-hash (list (cons #\( #\)) (cons #\[ #\]) (cons #\{ #\}) (cons #\< #\>))))

;; idea, when seeing open character push on to the expected stack what you expect to see next.
;; when you see something that matches what you expect you can pop that element off and continue

;; Returns true if closing and opening are the same type of chunks - i.e. ()'s or []'s
;; Character Character -> Boolean
(define (matches? closing opening)
  (char=? closing (hash-ref PAIRING opening)))

;; List<Characters> List<Characters> One of [Incomplete, Corrupt, Ok]
(define (line-type line expected)
  (define (front) (car line))
  (define (remaining) (cdr line))
  (cond [(and (null? line) (null? expected)) 'ok]
        [(and (null? line) (not (null? expected))) 'incomplete]
        [(set-member? OPENING (front)) (line-type (remaining) (cons (front) expected))]
        [(set-member? CLOSING (front)) (if (matches? (front) (car expected))
                                         (line-type (remaining) (cdr expected))
                                         (front))])
  )

;; Convert character to score
;; Character -> Integer
(define (score-corrupted character)
  (define comp (lambda (c) (char=? c character)))
  (cond [(comp #\)) 3]
        [(comp #\]) 57]
        [(comp #\}) 1197]
        [(comp #\>) 25137]))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define line-types (map (lambda (chunk-line) (line-type chunk-line '())) (map string->list lines)))
  (define only-corrupted (filter (lambda (result) (set-member? CLOSING result)) line-types))
  (define scores (map score-corrupted only-corrupted))
  (apply + scores) 
  )

(define filename (vector-ref (current-command-line-arguments) 0))
(with-input-from-file filename
                      solver
                      #:mode 'text
                      )
