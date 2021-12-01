#lang racket

;; List<Pair<Integer, Integer>> -> Number
(define (fn l1 l2 acc)
  (cond [(empty? l1) acc]
        [(> (first l1) (first l2)) (fn (rest l1) (rest l2) (add1 acc))]
        [(<= (first l1) (first l2)) (fn (rest l1) (rest l2) acc)]
        [else (raise (format "uh oh: ~a, ~a, acc"))]))
