#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

(define (increment-cell-builder n)
	(lambda (c) (add1 (modulo (+ (sub1 n) c) 9)))) 

;; Increments all numbers on a grid by n and wraps 10 to 0
;; Vec<Vec<Number> Number -> Vec<Vec<Number>
(define (increment-grid grid n)
  (map-grid (increment-cell-builder n) grid))

;; Creates a vector of n grids based on grid with each grid at index j
;; being the j-1th grid with "increment-grid" applied to it. The first
;; grid is unmodified and the nth grid has n-1 passed to increment-grid
;; Vec<Vec<Number>> Number -> Vec<Vec<Vec<Number>>>
(define (reference-grids grid n)
  (for/vector ([i (in-range n)])
            (increment-grid grid i)))

;; Replace each number in layout with the corresponding
;; index in references.
;; VecX4 is really a grid of subgrids.
;; Vec<Vec<Number>> Vec<Vec<Vec<Any>>> -> Vec<Vec<Vec<Vec<Any>>>>
(define (expand-tiles layout references)
  (map-grid (lambda (index) (vector-ref references index)) layout))

;; Takes a line of grids and merges all rows
;; Vec<Grid<Any>> -> Grid<Any>
(define (merge-sub-rows grids-in-row)
  (define first-grid (vector-ref grids-in-row 0))
  (define rows-per-grid (vector-length first-grid))
  ;; Collect all rows into new grid
  (for/vector ([row-i (in-range rows-per-grid)])
              ;; Merge ith row for grids from left-to-right
              (apply vector-append (map (lambda (g) (vector-ref g row-i)) (vector->list grids-in-row)))))

;; Merges rows of multiple grids. In an example
;; with 3 grids, each having 2 rows, the output
;; is 1 grid with 2 rows.
;; | 1 2 | | 3 4 | | 5 6 | --> | 1 2 3 4 5 6 |
;; | 7 8 | | 9 0 | | 1 2 | --> | 7 8 9 0 1 2 |

;; Row<Col<Grid<Any>>> -> Vec<Grid<Any>> or Row<Grid<Any>>
;; Grid is Vec<Vec<Any>> is Row<Col<Any>>
(define (merge-columns composite-grid) 
  (for/vector ([row-of-subgrids (in-vector composite-grid)])
              (merge-sub-rows row-of-subgrids)))

;; assume this function is applie after merge-columns
;;Row<Grid<Any>> -> Grid<Any>
(define (merge-rows composite-grid)
  ;; For each sub-grid (with 1 sub-grid per row)
  (sequence-fold vector-append #() (in-vector composite-grid)))

(define tests
  (test-suite
    "tests"
    (test-case "increment number by 1 and wrap at 9 back to 1"
               (check-equal? (max 1 (modulo 10 10))
                             1))

    (test-case "increment-entire grid and wrap-around at 9 to 1"
               (define the-grid (vector (vector 1 2 3)
                                        (vector 4 5 6)
                                        (vector 7 8 9)))
               (define expected (vector (vector 2 3 4)
                                        (vector 5 6 7)
                                        (vector 8 9 1)))
               (check-equal? (increment-grid the-grid 1)
                             expected))

    (test-case "generate array containing original grid and next 4 iterations"
               (define the-grid (vector (vector 8)))
               (define expected (vector (vector (vector 8))
                                        (vector (vector 9))
                                        (vector (vector 1))
                                        (vector (vector 2))))
               (check-equal? (reference-grids the-grid 4)
                             expected))


    (test-case "expand tile-map with sub-grids for single grid"
               (define tile-map (vector (vector 0)))
               (define references (vector (vector (vector 1))))
               (define expected (vector (vector (vector (vector 1)))))
               (check-equal? (expand-tiles tile-map references)
                             expected))

    (test-case "expand tile-map with sub-grids for multiple grids"
               (define tile-map (vector (vector 0 1)
                                        (vector 1 0)))
               (define references (vector (vector (vector 1))
                                          (vector (vector 2))))
               (define expected (vector (vector (vector (vector 1))
                                                (vector (vector 2)))
                                        (vector (vector (vector 2))
                                                (vector (vector 1)))))
               (check-equal? (expand-tiles tile-map references)
                             expected))

    (test-case "merge-columns merges simple row with 3 sub-grids: | 1 | 2 | 3 | -> | 1 2 3 |"
               ;;                Row     Col    Cell in Col
               (define expanded (vector (vector (vector (vector 1))
                                                (vector (vector 2))
                                                (vector (vector 3)))))
               (define expected (vector (vector (vector 1 2 3))))
               (check-equal? (merge-columns expanded)
                             expected))

    (test-case "merge-columns leaves meta-rows: see comments for shape"
               ;; | 1 2 | | 3 4 |      | 1 2 3 4 |
               ;; | 5 6 | | 7 8 |      | 5 6 7 8 |
               ;; |-----| |-----| ==>  |---------|
               ;; | 9 1 | | 2 3 |      | 9 1 2 3 |
               ;; | 4 5 | | 6 7 |      | 4 5 6 7 |
               (define expanded (vector (vector (vector (vector 1 2)
                                                        (vector 5 6))
                                                (vector (vector 3 4)
                                                        (vector 7 8)))
                                        (vector (vector (vector 9 1)
                                                        (vector 4 5))
                                                (vector (vector 2 3)
                                                        (vector 6 7)))))
               (define expected (vector (vector (vector 1 2 3 4)
                                                (vector 5 6 7 8))
                                        (vector (vector 9 1 2 3)
                                                (vector 4 5 6 7))))
               (check-equal? (merge-columns expanded)
                             expected))

    (test-case "merge-rows simple column with 3 sub-grids: see comments for shape"
               ;; |1 2|
               ;; |3 4|      |1 2|
               ;; ----- ===> |3 4|
               ;;            |5 6|
               ;; |5 6|      |7 8|
               ;; |7 8|
               (define expanded (vector (vector (vector 1 2)
                                                (vector 3 4))
                                        (vector (vector 5 6))
                                        (vector (vector 7 8))))
               (define expected (vector (vector 1 2)
                                        (vector 3 4)
                                        (vector 5 6)
                                        (vector 7 8)))
               (check-equal? (merge-rows expanded)
                             expected))

    ;; closes test-suite
    ))

(define unsuccessful-test-count (run-tests tests))

(define (solver) 
  (define risk-grid (map-grid (compose string->number string) (read-grid (current-input-port))))
  (define rows (vector-length risk-grid))
  (define cols (vector-length (vector-ref risk-grid 0)))
  (define references (reference-grids risk-grid 9))

  (define subarr (vector (vector 0 1 2 3 4)
                         (vector 1 2 3 4 5)
                         (vector 2 3 4 5 6)
                         (vector 3 4 5 6 7)
                         (vector 4 5 6 7 8)))

  (define expanded (expand-tiles subarr references))
  (define merged-cols (merge-columns expanded))
  (define merged (merge-rows merged-cols))
  (with-output-to-file "output.txt"
    (thunk (grid-print merged))
    #:mode 'text
    #:exists 'replace)

  #f)

(if (= unsuccessful-test-count 0)
  (setup-and-run solver)
  (begin
    (println "Tests failed. Skipping solver execution")
    (exit)))
