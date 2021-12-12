#lang racket
(require "../helpers.rkt")
(require data/queue)

(define width #f)
(define height #f)

;; Increment cells in rectangle (x0 y0) -> (x1 y1)
;; Integer Integer Integer Integer Vector<Vector<Number>> -> Void
;; TODO remove
(define (increment-subset! x0 y0 x1 y1 grid)
  (apply-grid-subset x0 y0 x1 y1 add1 grid))

;; Increments all cells 1
;; Vector<Vector<Number>> -> Vector<Vector<Number>>
(define (increment-all-cells! grid)
  (apply-grid add1 grid))

;; Posn -> Boolean
(define (valid-position? position)
  (define x (posn-x position))
  (define y (posn-y position))
  (and (<= 0 (posn-x position) (sub1 width))
       (<= 0 (posn-y position) (sub1 height))))

;; Gets valid coordinates in a flash zone
;; Coordinate -> List[Coordinate]
(define (get-3x3-coordinates c)
  (define x (posn-x c))
  (define y (posn-y c))
  (define maybe-neighbors `(,(make-posn (sub1 x) (sub1 y))
                              ,(make-posn x (sub1 y))
                              ,(make-posn (add1 x) (sub1 y))
                              ,(make-posn (sub1 x) y)
                              ,(make-posn x y)
                              ,(make-posn (add1 x) y)
                              ,(make-posn (sub1 x) (add1 y))
                              ,(make-posn x (add1 y))
                              ,(make-posn (add1 x) (add1 y))))
  (filter valid-position? maybe-neighbors))

;; Increments coordinate by 1
;; Grid Coordinate -> Void
(define (increment! grid pos)
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define current (coordinate-ref grid pos))
  (vector-set! (vector-ref grid y) x (add1 (cell-value current))))

;; Increments cells by 1
;; Grid List<Coordinates> -> Void
(define (flash-cells! grid cells)
  (for ([cell (in-list cells)])
       (increment! grid cell)))

;; Moves grid forward 1 and return number of flashes
;; Vector<Vector<Number>> -> Integer
(define (step! grid)
  (set! width (vector-length (vector-ref grid 0)))
  (set! height (vector-length grid))
  ;; increment all cells by 1
  (increment-all-cells! grid)

  ;; go through and add all cells with value 10 to cells-to-flash
  (define flash-coordinates (map cell-pos
                                 (filter cell-value
                                         (grid-to-coordinates (map-grid (curry = 10)
                                                                        grid)))))

  (define to-flash (make-queue))
  (for ([coord (in-list flash-coordinates)])
       (enqueue! to-flash coord))

  (define flash-count (length flash-coordinates))

  ;; The "do" form cannot be called without setting an
  ;; initial id. Because the init id is evaluted before 
  ;; the end-condition is checked this op needs to be
  ;; thrown away.
  ;; Now, the condition is checked before pulling the item off the queue. Not great but acceptable.
  ;; TODO rewrite the do-form into a proper while-form
  ;; that evaluates the condition before doing anything else.


  (do ([next null]) ;; leave this empty for "do" form to work
    ((queue-empty? to-flash)) ;; stop condition checked before next item is pulled
    (let* ([next (dequeue! to-flash)]
           [coordinates-to-inc (get-3x3-coordinates next)])

      ;; increment 3x3 grid of cells by 1
      (flash-cells! grid coordinates-to-inc)

      ;; filter on cells with exactly 10. Not more than 10 because those cells should
      ;; already be queued up to flash.
      (define must-flash (filter (lambda (e) (= 10 (cell-value e)))
                               (map (lambda (c) (coordinate-ref grid c)) coordinates-to-inc)))

      (set! flash-count (+ flash-count (length must-flash)))

      ;; those to flash this step to queue
      (for ([cell (in-list must-flash)])
           (enqueue! to-flash (cell-pos cell)))
    ))

  ;; TODO: if any cells exist with 10 remaing throw an error

  ;; set all cells greater than or equal to 10 to 0
  (define (reset-if-greater-than-10 val)
    (if (> val 10)
      0
      val))
  (apply-grid reset-if-greater-than-10 grid)

  flash-count
  )

;; Vector<Vector<Any>> -> Void
(define (print-grid grid)
  (for ([row (in-vector grid)])
       (println (vector->list row))))

(define (solver)
  (define current (map-grid (lambda (el) (string->number (string el))) (read-grid (current-input-port))))
  (define original (vector-deep-copy current))
  (define iterations 100)
  (define flashes (for/sum ([i (in-range iterations)])
       (step! current)))
  (print-grid original)
  (println (format "after ~a iterations" iterations))
  (print-grid current)
  (println (format "flashes ~a" flashes))
  #f
)

(setup-and-run solver)
