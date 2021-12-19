#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

(define-struct posn (col row) #:transparent)
(define-struct fold (axis location) #:transparent)

(define (string->posn line)
  (define coordinates (map string->number (string-split line ",")))
  (make-posn (first coordinates) (second coordinates))
  )

(define (string->fold line)
  (define split (map string-trim (string-split (substring line 10) "=")))
  (make-fold (first split) (string->number (second split))))

(define (coordinate? str)
  (define vals (map string->number (string-split str ",")))
  (and (< 0 (length vals))
       (not (is-fold? str))))

(define (is-fold? str)
  (string-prefix? str "fold along "))

(define (deduplicate l)
  (set->list (list->set l)))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define coordinates (map string->posn (filter coordinate? lines)))
  (define folds (map string->fold (filter is-fold? lines)))
  (println folds)
  (for ([fold (in-list (list (first folds)))])
       (define-values (above new-below) (map-y-below-fold coordinates (fold-location fold)))
       (println (format "above: ~a" above))
       (println (format "below ~a" new-below))
       (set! coordinates (reset-y-base (deduplicate (append above new-below))))
       (println (format "updated: ~a" coordinates))
       (println (format "visible dots: ~a" (length coordinates)))
       )
  )

(define (min-or-zero list-of-numbers)
  (apply min list-of-numbers))

(define (map-y current fold)
  (- (* 2 fold) current))

(define (map-posn-y p fold)
  (make-posn (posn-col p) (map-y (posn-row p) fold)))

(define (map-y-below-fold points fold)
  (define-values (above below) (partition (lambda (p) (< (posn-row p) fold)) points))
  (values above (map (lambda (p) (map-posn-y p fold)) below)))

(define (reset-y-base points)
  (define min-y (min-or-zero (map posn-row points)))
  (map (lambda (p) (make-posn (posn-col p)
                              (+ (posn-row p) (abs min-y))))
       points))

(define tests
  (test-suite
    "tests"
    (test-case "min-or-zero gets smallest neg"
               (define in (list -3 -1 -5 0 10))
               (check-equal? (min-or-zero in)
                             -5))
    (test-case "map-y y > fold but y-fold > 0 gives positive answer"
               (define current-y 8)
               (define fold-y 5)
               (define expected-y 2)
               (check-equal? (map-y current-y fold-y)
                             expected-y))

    (test-case "map-y y > fold but y-fold < 0 gives negative answer" 
               (define current-y 12)
               (define fold-y 5)
               (define expected-y -2)
               (check-equal? (map-y current-y fold-y)
                             expected-y))

    (test-case "map-y-below-fold only remaps y greater than or equal to fold"
               (define points `(
                                ,(make-posn 0 0)
                                ,(make-posn 0 1)
                                ,(make-posn 0 3)))
               (define fold 2)
               (define-values (before after) (map-y-below-fold points fold))
               (check-equal? (length after)
                             1)
               (check-equal? (first after)
                             (make-posn 0 1)))

    (test-case "map-y-below-fold remaps y to negatvies"
               (define points `(
                                ,(make-posn 0 1)
                                ,(make-posn 0 2)
                                ,(make-posn 0 3)
                                ,(make-posn 0 4)
                                ,(make-posn 0 6)
                                ,(make-posn 0 7)
                                ,(make-posn 0 8)
                                ,(make-posn 0 9)
                                ,(make-posn 0 10)
                                ,(make-posn 0 11)
                                ,(make-posn 0 12)
                                ,(make-posn 0 13)
                                ,(make-posn 0 14)
                                ))
               (define y-fold 5)
               (define expected-above-points `(
                                               ,(make-posn 0 1)
                                               ,(make-posn 0 2)
                                               ,(make-posn 0 3)
                                               ,(make-posn 0 4)))
               (define expected-below-points `(
                                               ,(make-posn 0 4)
                                               ,(make-posn 0 3)
                                               ,(make-posn 0 2)
                                               ,(make-posn 0 1)
                                               ,(make-posn 0 0)
                                               ,(make-posn 0 -1)
                                               ,(make-posn 0 -2)
                                               ,(make-posn 0 -3)
                                               ,(make-posn 0 -4)))
               (define-values (before after) (map-y-below-fold points y-fold))
               (check-equal? before expected-above-points)
               (check-equal? after expected-below-points))

    (test-case "reset-y-base bumps all y's by abs of min y"
               (define points `(
                                ,(make-posn 0 4)
                                ,(make-posn 0 3)
                                ,(make-posn 0 2)
                                ,(make-posn 0 1)
                                ,(make-posn 0 0)
                                ,(make-posn 0 -1)
                                ,(make-posn 0 -2)
                                ,(make-posn 0 -3)
                                ,(make-posn 0 -4)))
               (define expected-points `(
                                ,(make-posn 0 8)
                                ,(make-posn 0 7)
                                ,(make-posn 0 6)
                                ,(make-posn 0 5)
                                ,(make-posn 0 4)
                                ,(make-posn 0 3)
                                ,(make-posn 0 2)
                                ,(make-posn 0 1)
                                ,(make-posn 0 0)))
               (check-equal? (reset-y-base points)
                             expected-points)
               )

    ;; closes test-suite
    ))

(run-tests tests)

(setup-and-run solver)
