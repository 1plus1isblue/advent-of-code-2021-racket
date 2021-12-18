#lang racket
(require racket/draw)
(require data/queue)
(define A-VERY-LARGE-NUMBER 10)
(define visited? #f)
(define next #f)
(define heightmap #f)
(define columns #f)
(define rows #f)
(define wall-or-not-wall #f)
(define-struct point (x y) #:transparent)

;; HeightMap is Vector<Vector<Number>>

;; Reads heightmap from stdin and outputs a lookup table
;; Void -> Vector<Vector<Number>>
(define (read-heightmap)
  (define character-lists (map string->list (port->lines (current-input-port))))
  (define (char->number c)
    (string->number (string c)))
  (define heights-by-rows (map (lambda (l) (map char->number l)) character-lists))
  (list->vector (map list->vector heights-by-rows)))

;; origin is top-left corner. x grows to right and y grows down.
(define (get-cell heightmap x y)
  (define width (vector-length (vector-ref heightmap 0)))
  (define height (vector-length heightmap))
  (if (and (<= 0 x (sub1 width))
           (<= 0 y (sub1 height)))
    (vector-ref (vector-ref heightmap y) x)
    A-VERY-LARGE-NUMBER))

;; Recurisvely run through each cell and if it's lower than it's neighbors return it as a low-point
;; Cases:
;; * center with 4 neighbors
;; * right of heightmap
;; * below heightmap
;; HeightMap Integer Integer -> List<Integer>
(define (all-lowpoints heightmap x y)
  (all-lowpoints-below heightmap x y))

;; Determines if target is less than all adjacent cells. Returns the value itself or -1
;; Number [Number, Number, Number, Number] -> Number
(define (is-min? target others)
  (if (and (< target (first others))
       (< target (second others))
       (< target (third others))
       (< target (fourth others)))
    target
    -1))

(define (all-lowpoints-right-of heightmap x y)
  (define width (vector-length (vector-ref heightmap 0)))
  (define height (vector-length heightmap))
  (define value-at (curry get-cell heightmap))
  (cond [(>= x width) null]
        [else
          (cons (is-min? (value-at x y)
                         `(,(value-at (sub1 x) y)
                            ,(value-at (add1 x) y)
                            ,(value-at x (sub1 y))
                            ,(value-at x (add1 y))))
                (all-lowpoints-right-of heightmap (add1 x) y))
          ]))

;; HeightMap Integer Integer -> List<Integer>
(define (all-lowpoints-below heightmap x y)
  (define height (vector-length heightmap))
  (cond [(>= y height) null]
        [else
          (append (all-lowpoints-right-of heightmap x y)
                  (all-lowpoints-below heightmap x (add1 y)))]))

(define (render heightmap)
  (define width (vector-length (vector-ref heightmap 0)))
  (define height (vector-length heightmap))
  (define BOX-SIZE 10)
  (define target (make-bitmap (* 2 BOX-SIZE width) (* 2 BOX-SIZE height)))
  (define dc (new bitmap-dc% [bitmap target]))
  (define color-map (make-hash (list (cons 0 (make-object color% 34 83 102))
                                     (cons 1 (make-object color% 235 127 0)))))
  
  #;
  (define color-map (make-hash (list (cons 0 (make-object color% 34 83 102))
                                     (cons 1 (make-object color% 34 83 102))
                                     (cons 2 (make-object color% 22 149 163))
                                     (cons 3 (make-object color% 22 149 163))
                                     (cons 4 (make-object color% 172 240 242))
                                     (cons 5 (make-object color% 172 240 242))
                                     (cons 6 (make-object color% 243 255 226))
                                     (cons 7 (make-object color% 243 255 226))
                                     (cons 8 (make-object color% 235 127 0))
                                     (cons 1 (make-object color% 0 0 0)))))
  (for ([row (in-range height)])
       (for ([col (in-range width)])
            (define x (* col BOX-SIZE))
            (define y (* row BOX-SIZE))
            (define cell-value (get-cell heightmap col row))
            (send dc set-brush (hash-ref color-map cell-value) 'solid)
            (send dc draw-rectangle x y BOX-SIZE BOX-SIZE)
            ;(send dc set-text-foreground "red")
            ;(send dc draw-text (number->string cell-value) (* col BOX-SIZE) (* row BOX-SIZE))
            ))
  (send target save-file "box.png" 'png))

;; origin is at (0,0). X grows to right. Y grows down.
(define (set-visited! x y val)
  (vector-set! (vector-ref visited? y) x val))

;; marks point, p, as visited
(define (visit-point! p)
  (set-visited! (point-x p) (point-y p) 3))

(define (is-1? x y)
  (= 1 (get-cell wall-or-not-wall x y)))

(define (visited-point? p)
  (vector-ref (vector-ref visited? (point-y p)) (point-x p)))

;; Vector<Vector<Any>> (Any . -> . Any) -> Vector<Vector<Any>>
(define (grid-map grid-vector proc)
  (vector-map (lambda (row) (vector-map proc row)) grid-vector))

;; Returns if the point is valid and unvisited
;; Point -> Boolean
(define (valid-point? p)
  (and (<= 0 (point-x p) (sub1 columns))
       (<= 0 (point-y p) (sub1 rows))
       (not (visited-point? p))
       ))

;; Get valid adjacent points
;; Point -> List<Point>
(define (adjacent-points p)
  (define left (make-point (sub1 (point-x p)) (point-y p)))
  (define right (make-point (add1 (point-x p)) (point-y p)))
  (define top (make-point (point-x p) (sub1 (point-y p))))
  (define bottom (make-point (point-x p) (add1 (point-y p))))
  (define result (filter valid-point? `(,left ,right ,top ,bottom)))
  result
  )

;; Returns number of points in basin. Assumes no walls are considered.
;; List<Point>
(define (count-flood-fill next-points)
  (define front (lambda () (car next-points)))
  (define remaining (lambda () (cdr next-points)))
  (cond [(null? next-points) 0]
        [(visited-point? (front)) (count-flood-fill (remaining))]
        [else (begin
                (visit-point! (front))
                (+ 1 (count-flood-fill (append (adjacent-points (front)) (remaining)))))]))

;; Gets list of all basin sizes
;; List<Point> -> List<Number>
(define (get-all-basins coordinates)
  (define front (lambda () (car coordinates)))
  (define remaining (lambda () (cdr coordinates)))
  (cond [(null? coordinates) null]
        [(visited-point? (front)) (get-all-basins (remaining))]
        [else (cons (count-flood-fill `(,(front)))
                 (get-all-basins (remaining)))]))

(define (solver)
  (set! heightmap (read-heightmap))
  (set! columns (vector-length (vector-ref heightmap 0)))
  (set! rows (vector-length heightmap))
  (set! wall-or-not-wall (grid-map heightmap (lambda (height) (if (< height 9) 0 1))))
  (set! visited? (grid-map heightmap (lambda (height) (>= height 9))))
  (define coordinates (flatten (for/list ([row (in-range rows)])
                                         (for/list ([col (in-range columns)])
                                                   (make-point col row)))))
  (set! coordinates (filter (lambda (coord) (not (is-1? (point-x coord) (point-y coord)))) coordinates))
  (apply * (take (sort (get-all-basins coordinates) >) 3))
  )

(define filename (vector-ref (current-command-line-arguments) 0))
(with-input-from-file filename 
                      solver
                      #:mode 'text)
