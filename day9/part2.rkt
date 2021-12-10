#lang racket
(require racket/draw)
(define A-VERY-LARGE-NUMBER 10)

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
                                     (cons 1 (make-object color% 34 83 102))
                                     (cons 2 (make-object color% 22 149 163))
                                     (cons 3 (make-object color% 22 149 163))
                                     (cons 4 (make-object color% 172 240 242))
                                     (cons 5 (make-object color% 172 240 242))
                                     (cons 6 (make-object color% 243 255 226))
                                     (cons 7 (make-object color% 243 255 226))
                                     (cons 8 (make-object color% 235 127 0))
                                     (cons 9 (make-object color% 0 0 0)))))
  (for ([row (in-range height)])
       (for ([col (in-range width)])
            (define x (* col BOX-SIZE))
            (define y (* row BOX-SIZE))
            (define cell-value (get-cell heightmap col row))
            (send dc set-brush (hash-ref color-map cell-value) 'solid)
            (send dc set-pen (hash-ref color-map cell-value) 0 'solid)
            (send dc draw-rectangle x y BOX-SIZE BOX-SIZE)
            ;(send dc set-text-foreground "red")
            ;(send dc draw-text (number->string cell-value) (* col BOX-SIZE) (* row BOX-SIZE))
            ))
  (send target save-file "box.png" 'png))

(define (solver)
  (define heightmap (read-heightmap))
  (render heightmap)
  ;; For each cell see if adjacent cells are higher
  ;; Assumption: No low-points are adjacent to one another. Can check this in input.
  ;(define low-points (all-lowpoints heightmap 0 0))
  ;(apply + (map add1 (filter-not negative? low-points)))
  )

(define filename (vector-ref (current-command-line-arguments) 0))
(with-input-from-file filename 
                      solver
                      #:mode 'text)
