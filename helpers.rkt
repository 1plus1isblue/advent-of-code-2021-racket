#lang racket
(provide count-increases
         recursive-list-map)

;; count adjacent values that increase by 2. Counts from beginning to end (left to right)
;; List<Pair<Integer, Integer>> -> Number
(define (count-increases a-list)
  (count-increases-helper a-list (cons 0 a-list) -1))

(define (count-increases-helper l1 l2 acc)
  (cond [(empty? l1) acc]
        [(> (first l1) (first l2))
         (count-increases-helper (rest l1) (rest l2) (add1 acc))]
        [(<= (first l1) (first l2))
         (count-increases-helper (rest l1) (rest l2) acc)]
        [else (raise (format "uh oh: ~a, ~a, acc"))]))

;; Recurisvely traverse a nested list and apply fn to any non-list elements encountered.
;; Assumes lists and does not work with vectors, sets, etc. 
;; (any -> any) ListOfAny -> ListOfAny
(define (recursive-list-map fn a-list)
  (cond [(null? a-list) null]
        [(list? (car a-list))
         (cons (recursive-list-map fn (car a-list))
               (recursive-list-map fn (cdr a-list)))]
        [(not (list? (car a-list)))
         (cons (fn (car a-list)) (recursive-list-map fn (cdr a-list)))]))
