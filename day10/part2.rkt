#lang racket
(require math/statistics)

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
        [(and (null? line) (not (null? expected))) expected]
        [(set-member? OPENING (front)) (line-type (remaining) (cons (front) expected))]
        [(set-member? CLOSING (front)) (if (matches? (front) (car expected))
                                         (line-type (remaining) (cdr expected))
                                         (front))])
  )

;; Convert incorrect character to score
;; Character -> Integer
(define (score-corrupted character)
  (define comp (lambda (c) (char=? c character)))
  (cond [(comp #\)) 3]
        [(comp #\]) 57]
        [(comp #\}) 1197]
        [(comp #\>) 25137]))

;; The chunk characters are backwards based on how  (line-type ...) constructs the expected characters.
;; Character -> Number
(define (incompleted-lookup character)
  (define comp (lambda (c) (char=? c character)))
  (cond [(comp #\() 1]
        [(comp #\[) 2]
        [(comp #\{) 3]
        [(comp #\<) 4]))

;; Convert auto-complete characters to score. Total is an accumulator of the current score
;; List<Characters> Number -> Number
(define (score-incompleted missing-characters total)
  (cond [(null? missing-characters) total]
        [else (score-incompleted (cdr missing-characters) (+ (incompleted-lookup (car missing-characters))
                                                             (* total 5)))]))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define line-types (map (lambda (chunk-line) (line-type chunk-line '())) (map string->list lines)))
  (define only-incompleted (filter list? line-types))
  (define only-corrupted (filter (lambda (result) (set-member? CLOSING result)) line-types))
  (median < (map (lambda (missing) (score-incompleted missing 0)) only-incompleted))
  )

(define filename (vector-ref (current-command-line-arguments) 0))
(with-input-from-file filename
                      solver
                      #:mode 'text
                      )
