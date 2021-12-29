#lang racket
(require "../helpers.rkt"
         graph
         rackunit
         rackunit/text-ui)

;; Gets minimal total cost for a path to (x,y) based
;; on the risks grid
(define lookup (make-hash))
(define (min-total-cost risks x y)
  (if (hash-has-key? lookup `(,x ,y))
    (hash-ref lookup `(,x ,y))
    (begin 
      (hash-set! lookup
                 `(,x ,y)
                 (let ([xy-risk (grid-ref x y risks)])
                   (cond [(and (= x 0) (= y 0)) (grid-ref x y risks)]
                         [(= x 0) (+ xy-risk (min-total-cost risks x (sub1 y)))] ; left-wall
                         [(= y 0) (+ xy-risk (min-total-cost risks (sub1 x) y))] ; top-wall
                         [else (+ xy-risk (min (min-total-cost risks (sub1 x) y)
                                              (min-total-cost risks x (sub1 y))))])))
      (hash-ref lookup `(,x ,y)))))

(define-struct edge (cost from to) #:transparent)

(define (get-edge x0 y0 x1 y1 grid)
  (if (has-ref? x1 y1 grid)
    (make-edge (grid-ref x1 y1 grid) `(,x0 ,y0) `(,x1 ,y1))
    #f))

(define (edge->list an-edge)
  `(,(edge-cost an-edge) ,(edge-from an-edge) ,(edge-to an-edge)))

;; Vec<Vec<Number>> -> Weighted/UndirectedGraph
(define (grid->graph grid)
  (define column-count (vector-length (vector-ref grid 0)))
  (define row-count (vector-length grid))
  (define edges (filter-not false?  (flatten (for/list ([x (in-range column-count)])
       (for/list ([y (in-range row-count)])
            `(,(get-edge x y (add1 x) y grid)
              ,(get-edge x y x (add1 y) grid)
              ,(get-edge x y (sub1 x) y grid)
              ,(get-edge x y x (sub1 y) grid)))))))
  (weighted-graph/directed (map edge->list edges)))

(define (shortest-path predecessors start end)
  (define path '())
  (do ([current end (hash-ref predecessors current)])
    ((equal? start current) (set! path (cons current path)))
    (set! path (cons current path)))
  path)

(define (print-path path the-grid)
  (define path-members (list->set path))
  (define rows (vector-length the-grid))
  (define cols (vector-length (vector-ref the-grid 0)))
  (for ([y (in-range rows)])
       (for ([x (in-range cols)])
            (if (set-member? path-members `(,x ,y))
                (display (cell-value (coordinate-ref the-grid (make-posn x y))))
                (display " ")
                )
            )
       (display "\n")
       (flush-output)
       )
  )

(define (edge->weight the-graph edge)
  `(,(edge-weight the-graph (first edge) (second edge)) ,(first edge) ,(second edge)))

(define tests
  (test-suite
    "tests"

    ;; TODO this is going to break
    (test-case "grid->graph"
               (define the-grid (vector (vector 0 1)
                                        (vector 1 2)))

               (define expected (weighted-graph/directed '(
                                                             (1 (0 0) (1 0))
                                                             (1 (0 0) (0 1))
                                                             (0 (1 0) (0 0))
                                                             (0 (0 1) (0 0))
                                                             (2 (1 0) (1 1))
                                                             (2 (0 1) (1 1))
                                                             (1 (1 1) (1 0))
                                                             (1 (1 1) (0 1))
                                                             )))
               (define actual (grid->graph the-grid))
               (check-equal? actual expected))

    (test-case "use dijkstra's"
               (define the-grid (vector (vector 0 1 3)
                                        (vector 1 2 2)))
               (define the-graph (grid->graph the-grid))
               (define-values (distance-costs predecessors) (dijkstra the-graph '(0 0)))
               (check-equal? (hash-ref distance-costs '(2 1))
                             5))

    (test-case "get shortest-path"
               (define the-grid (vector (vector 0 1 3)
                                        (vector 2 2 2)))
               (define the-graph (grid->graph the-grid))
               (define-values (distance-costs predecessors) (dijkstra the-graph '(0 0)))
               (check-equal? (shortest-path predecessors '(0 0) '(2 1))
                             '((0 0) (1 0) (1 1) (2 1))))

    ;; closes test-suite
    ))

(define unsuccessful-test-count (run-tests tests))


(define (solver)
  (define risk-grid (map-grid (compose string->number string) (read-grid (current-input-port))))
  (grid-set! 0 0 risk-grid 0)
  (define rows (vector-length risk-grid))
  (define cols (vector-length (vector-ref risk-grid 0)))
  (define the-graph (grid->graph risk-grid))
  (define-values (costs preds) (dijkstra the-graph '(0 0)))
  (print-path (shortest-path preds '(0 0) `(,(sub1 cols) ,(sub1 rows))) risk-grid)
  (define specific-edge->weight (curry edge->weight the-graph))
  #;
  (println (format "edge-weights: ~a" (sort (map specific-edge->weight (get-edges the-graph))
                                            (lambda (v1 v2) (and (< (second (second v1)) (second (second v2)))
                                                                 (< (first (second v1)) (first (second v2))))))))

  (println (format "cost of minimal path from (0 0) to ~a: ~a" `(,(sub1 cols) ,(sub1 rows)) (hash-ref costs `(,(sub1 cols) ,(sub1 rows)))))
  #f)

(if (= unsuccessful-test-count 0)
  (setup-and-run solver)
  (begin
    (println "Tests failed. Skipping solver execution")
    (exit)))
