#lang racket
(provide count-increases)

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
