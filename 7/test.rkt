#lang racket
(require math/statistics)


(define sorted-list (sort '(1000000 1 2 0 4 2 7 1 2 14) <))
(median < sorted-list)
