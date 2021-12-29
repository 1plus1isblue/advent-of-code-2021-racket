#lang racket
(provide count-increases
         recursive-list-map
         setup-and-run 
         read-grid
         map-grid
         coordinate-ref
         grid-ref
         grid-set!
         apply-grid
         apply-grid-subset
         grid-to-coordinates
         make-posn
         posn-x
         posn-y
         cell-value
         vector-deep-copy
         memoize-1
         memoize-2
         grid-print 
         has-ref?
         )

(define-struct posn (x y) #:transparent)
(define-struct cell (pos value) #:transparent)

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

;; Reads remaining lines as a 2d vector grid. Assumes grid
;; is a 2d grid of characters
;; Port -> Vector<Vector<Any>>
(define (read-grid in)
  (define list-of-lines (map string->list (port->lines in)))
  (list->vector (map list->vector list-of-lines)))


;; Applies a function to each cell of a grid generating a new grid
;; (Any . -> . Any) Vector<Vector<Any>> -> Vector<Vector<Any>>
(define (map-grid proc grid)
  (define apply-to-row (lambda (row) (vector-map proc row)))
  (vector-map apply-to-row grid))

;; Assume (x0, y0) & (x1, y1) are two corners off a rectangle
;; with (x0, y0) in the top left and (x1, y1) in the top right.
;; Assume (x0, y0) & (x1, y1) are valid points in the grid
(define (apply-grid-subset x0 y0 x1 y1 proc grid)
  (for ([row (in-range y0 (add1 y1))]) 
       (for ([col (in-range x0 (add1 x1))])
            (define cell (vector-ref (vector-ref grid row) col))
            (vector-set! (vector-ref grid row) col (proc cell)))))

;;Apply a function to every cell in a grid updating it in place
;; (Any . -> . Any) Vector<Vector<Any>> -> Void
(define (apply-grid proc grid)
  (define width (vector-length (vector-ref grid 0)))
  (define height (vector-length grid))
  (for ([row (in-range height)])
       (for ([col (in-range width)])
            (define cell (vector-ref (vector-ref grid row) col))
            (vector-set! (vector-ref grid row) col (proc cell))))
  )

;; Converts a 2d grid to a list of elements that contain a coordinate and the cell's value. The coordinate system uses top-left as the origin. The x axis grows to the right and the y-axis grows down.
;; Vector<Vector<Any>> -> ListOf[(Posn, Any)]
(define (grid-to-coordinates grid) 
  (define width (vector-length (vector-ref grid 0)))
  (define height (vector-length grid))
  (define result '())
  (for ([row (in-range height)])
       (for ([col (in-range width)])
            (define value (vector-ref (vector-ref grid row) col))
            (define new-el (make-cell (make-posn col row) value))
            (set! result (cons new-el result))))
  result)

;; Boilerplate for setting up and running solvers
(define (setup-and-run solver)
  (define cmd-args (current-command-line-arguments))
  (if (= (vector-length cmd-args) 0)
    (begin (println "No filename passed to program. Exiting")
           (exit))
    (with-input-from-file (vector-ref cmd-args 0)
                          solver
                          #:mode 'text)
    )
  )


;; Access value in grid at coordinate
;; Grid Posn -> Cell
(define (coordinate-ref grid coordinate)
  (define x (posn-x coordinate))
  (define y (posn-y coordinate))
  (make-cell (make-posn x y) (grid-ref x y grid)))

;; Access cell at location in grid
;; Integer Integer Vector<Vector<Any>> -> Any
(define (grid-ref x y grid)
  (define width (vector-length (vector-ref grid 0)))
  (define height (vector-length grid))
  (if (and (<= 0 x (sub1 width))
           (<= 0 y (sub1 height)))
    (vector-ref (vector-ref grid y) x)
    (raise (format "~a ~a is invalid location in grid" x y)))
  )

;; Says whether or not a location is valid
;; Integer Integer Vector<Vector<Any>> -> Boolean
(define (has-ref? x y grid)
  (define width (vector-length (vector-ref grid 0)))
  (define height (vector-length grid))
  (and (<= 0 x (sub1 width))
       (<= 0 y (sub1 height))))

;; Set cell at location in grid
;; TODO test
;; Integer Integer Vector<Vector<Any>> Any -> Void
(define (grid-set! x y grid value)
  (vector-set! (vector-ref grid y) x value))

;; Copies a 2d vector
(define (vector-deep-copy vec)
  (for/vector ([row (in-vector vec)])
              (vector-copy row)))


;; Must be used with (set! f (memoize-1 f)) in the calling site
;; TODO replace with a macro.
;; See https://stackoverflow.com/a/66304604 for more info
(define (memoize-1 f)
	(let ((lookup (make-hash)))
		(lambda (x)
			(unless (hash-has-key? lookup x)
				(hash-set! lookup x (f x)))
			(hash-ref lookup x))))


;; Must be used with (set! f (memoize-1 f)) in the calling site
;; TODO replace with a macro.
;; See https://stackoverflow.com/a/66304604 for more info
(define (memoize-2 f)
	(let ((lookup (make-hash)))
		(lambda (x y)
			(unless (hash-has-key? lookup `(,x ,y))
				(hash-set! lookup `(,x ,y) (f x y)))
			(hash-ref lookup `(,x ,y)))))

;; Vec<Vec<Any>>
(define (grid-print grid)
  (for ([row (in-vector grid)])
       (for ([cell (in-vector row)])
            (display cell))
       (display "\n")
       (flush-output)))
