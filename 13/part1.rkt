#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

(define-struct posn (col row) #:transparent)
(define-struct fold (axis location) #:transparent)

(define (sort-points points)
  (sort points
        (lambda (p1 p2)
          (and (< (posn-col p1) (posn-col p2))
               (< (posn-row p1) (posn-row p2))))))

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

(define (handle-y-fold coordinates location)
  (define-values (above new-below) (map-y-below-fold coordinates location))
  (define non-normalized-coordinates (reset-y-base (deduplicate (append above new-below))))
  non-normalized-coordinates)

(define (handle-x-fold coordinates location)
  (define-values (left right) (map-x-right-fold coordinates location))
  (define non-normalized-coordinates (reset-x-base (deduplicate (append left right))))
  non-normalized-coordinates)

(define (iter coordinates the-fold)
  (if (string=? (fold-axis the-fold) "y")
    (handle-y-fold coordinates (fold-location the-fold))
    (handle-x-fold coordinates (fold-location the-fold)))
  )

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define coordinates (map string->posn (filter coordinate? lines)))
  (define folds (map string->fold (filter is-fold? lines)))
  (for ([fold (in-list folds)])
             (set! coordinates (iter coordinates fold)))
  (println (length coordinates))
  (println coordinates)
  )

(define (min-or-zero list-of-numbers)
  (apply min list-of-numbers))

(define (map-y current fold)
  (- (* 2 fold) current))

(define (map-x current fold)
  (- (* 2 fold) current))

(define (map-posn-y p fold)
  (make-posn (posn-col p) (map-y (posn-row p) fold)))

(define (map-posn-x p fold)
  (make-posn (map-x (posn-col p) fold) (posn-row p)))


(define (map-y-below-fold points fold)
  (define-values (above below) (partition (lambda (p) (< (posn-row p) fold)) points))
  (values above (map (lambda (p) (map-posn-y p fold)) below)))

(define (map-x-right-fold points fold)
  (define-values (left right) (partition (lambda (p) (< (posn-col p) fold)) points))
  (values left (map (lambda (p) (map-posn-x p fold)) right)))


(define (reset-y-base points)
  (define min-y (min-or-zero (map posn-row points)))
  (map (lambda (p) (make-posn (posn-col p)
                              (+ (posn-row p) (abs min-y))))
       points))

(define (reset-x-base points)
  (define min-x (min-or-zero (map posn-col points)))
  (map (lambda (p) (make-posn  (+ (posn-col p) (abs min-x))
                               (posn-row p)))
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

    (test-case "reset-y-base doesn't change xs"
               (define points `(
                                ,(make-posn 1 4)
                                ,(make-posn 2 3)
                                ,(make-posn 3 2)
                                ,(make-posn 4 1)
                                ,(make-posn 5 0)
                                ,(make-posn 6 -1)
                                ,(make-posn 7 -2)
                                ,(make-posn 8 -3)
                                ,(make-posn 9 -4)))
               (define expected-points `(
                                         ,(make-posn 5 4)
                                         ,(make-posn 4 5)
                                         ,(make-posn 3 6)
                                         ,(make-posn 2 7)
                                         ,(make-posn 1 8)
                                         ,(make-posn 6 3)
                                         ,(make-posn 7 2)
                                         ,(make-posn 8 1)
                                         ,(make-posn 9 0)))
               (check-equal? (list->set (reset-y-base points))
                             (list->set expected-points))
               )

    (test-case "iter on input data"
               (define points `(
                                ,(make-posn 6 10)
                                ,(make-posn 0 14)
                                ,(make-posn 9 10)
                                ,(make-posn 0 3)
                                ,(make-posn 10 4)
                                ,(make-posn 4 11)
                                ,(make-posn 6 0)
                                ,(make-posn 6 12)
                                ,(make-posn 4 1)
                                ,(make-posn 0 13)
                                ,(make-posn 10 12)
                                ,(make-posn 3 4)
                                ,(make-posn 3 0)
                                ,(make-posn 8 4)
                                ,(make-posn 1 10)
                                ,(make-posn 2 14)
                                ,(make-posn 8 10)
                                ,(make-posn 9 0)
                                ))
               (define expected-points `(
                                         ,(make-posn 0 0)
                                         ,(make-posn 2 0)
                                         ,(make-posn 3 0)
                                         ,(make-posn 6 0)
                                         ,(make-posn 9 0)
                                         ,(make-posn 0 1)
                                         ,(make-posn 4 1)
                                         ,(make-posn 6 2)
                                         ,(make-posn 10 2)
                                         ,(make-posn 0 3)
                                         ,(make-posn 4 3)
                                         ,(make-posn 1 4)
                                         ,(make-posn 3 4)
                                         ,(make-posn 6 4)
                                         ,(make-posn 8 4)
                                         ,(make-posn 9 4)
                                         ,(make-posn 10 4)
                                         ))
               (define actual-points (iter points (make-fold "y" 7)))
               (define actual-set (list->set actual-points))
               (define expected-set (list->set expected-points))
               (check-equal? actual-set expected-set
                             (format "found ~a and missing ~a"
                                     (set-subtract actual-set expected-set)
                                     (set-subtract expected-set actual-set)))
               )

    (test-case "iter second step of input"
               (define points `(
                                ,(make-posn 0 0)
                                ,(make-posn 2 0)
                                ,(make-posn 3 0)
                                ,(make-posn 6 0)
                                ,(make-posn 9 0)
                                ,(make-posn 0 1)
                                ,(make-posn 4 1)
                                ,(make-posn 6 2)
                                ,(make-posn 10 2)
                                ,(make-posn 0 3)
                                ,(make-posn 4 3)
                                ,(make-posn 1 4)
                                ,(make-posn 3 4)
                                ,(make-posn 6 4)
                                ,(make-posn 8 4)
                                ,(make-posn 9 4)
                                ,(make-posn 10 4)
                                ))
               (define expected-points `(
                                         ,(make-posn 0 0)
                                         ,(make-posn 1 0)
                                         ,(make-posn 2 0)
                                         ,(make-posn 3 0)
                                         ,(make-posn 4 0)
                                         ,(make-posn 0 1)
                                         ,(make-posn 4 1)
                                         ,(make-posn 0 2)
                                         ,(make-posn 4 2)
                                         ,(make-posn 0 3)
                                         ,(make-posn 4 3)
                                         ,(make-posn 0 4)
                                         ,(make-posn 1 4)
                                         ,(make-posn 2 4)
                                         ,(make-posn 3 4)
                                         ,(make-posn 4 4)
                                         ))
               (define actual-points (iter points (make-fold "x" 5)))
               (define actual-set (list->set actual-points))
               (define expected-set (list->set expected-points))
               (check-equal? actual-set expected-set
                             (format "found ~a and missing ~a"
                                     (set-subtract actual-set expected-set)
                                     (set-subtract expected-set actual-set)))
               )

    ;; closes test-suite
    ))

(run-tests tests)

(setup-and-run solver)
