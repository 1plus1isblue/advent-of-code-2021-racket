#lang racket
(require racket/draw)

(define-struct posn (col row) #:transparent)

(define points `(
                 ,(posn 12 3)
                 ,(posn 31 0)
                 ,(posn 38 1)
                 ,(posn 23 0)
                 ,(posn 13 2)
                 ,(posn 30 1)
                 ,(posn 26 5)
                 ,(posn 35 4)
                 ,(posn 18 5)
                 ,(posn 28 3)
                 ,(posn 15 0)
                 ,(posn 7 0)
                 ,(posn 37 2)
                 ,(posn 10 5)
                 ,(posn 35 3)
                 ,(posn 22 0)
                 ,(posn 18 4)
                 ,(posn 27 3)
                 ,(posn 38 0)
                 ,(posn 13 1)
                 ,(posn 10 4)
                 ,(posn 36 2)
                 ,(posn 11 3)
                 ,(posn 33 5)
                 ,(posn 35 2)
                 ,(posn 18 3)
                 ,(posn 10 3)
                 ,(posn 25 4)
                 ,(posn 0 5)
                 ,(posn 28 1)
                 ,(posn 33 4)
                 ,(posn 12 0)
                 ,(posn 8 4)
                 ,(posn 35 1)
                 ,(posn 18 2)
                 ,(posn 10 2)
                 ,(posn 25 3)
                 ,(posn 2 2)
                 ,(posn 15 5)
                 ,(posn 0 4)
                 ,(posn 7 5)
                 ,(posn 33 3)
                 ,(posn 8 3)
                 ,(posn 38 5)
                 ,(posn 27 0)
                 ,(posn 23 4)
                 ,(posn 0 3)
                 ,(posn 30 5)
                 ,(posn 35 0)
                 ,(posn 1 2)
                 ,(posn 18 1)
                 ,(posn 10 1)
                 ,(posn 6 5)
                 ,(posn 25 2)
                 ,(posn 17 2)
                 ,(posn 15 4)
                 ,(posn 32 3)
                 ,(posn 3 0)
                 ,(posn 33 2)
                 ,(posn 22 5)
                 ,(posn 11 0)
                 ,(posn 8 2)
                 ,(posn 38 4)
                 ,(posn 23 3)
                 ,(posn 0 2)
                 ,(posn 30 4)
                 ,(posn 13 5)
                 ,(posn 25 1)
                 ,(posn 21 5)
                 ,(posn 2 0)
                 ,(posn 15 3)
                 ,(posn 31 3)
                 ,(posn 33 1)
                 ,(posn 16 2)
                 ,(posn 26 0)
                 ,(posn 18 0)
                 ,(posn 8 1)
                 ,(posn 38 3)
                 ,(posn 23 2)
                 ,(posn 0 1)
                 ,(posn 30 3)
                 ,(posn 13 4)
                 ,(posn 28 5)
                 ,(posn 15 2)
                 ,(posn 5 4)
                 ,(posn 1 0)
                 ,(posn 8 0)
                 ,(posn 38 2)
                 ,(posn 23 1)
                 ,(posn 0 0)
                 ,(posn 30 2)
                 ,(posn 13 3)
                 ,(posn 32 0)
                 ,(posn 28 4)
                 ,(posn 15 1)
                 ,(posn 35 5)
                 ,(posn 20 4)
                 ,(posn 27 5)
                 ))


(define (render positions)
  (define width 200)
  (define height 200)
  (define BOX-SIZE 5)
  (define target (make-bitmap (* 2 BOX-SIZE width) (* 2 BOX-SIZE height)))
  (define dc (new bitmap-dc% [bitmap target]))
  (define color (make-object color% 255 0 0))
  (for ([cell (in-list positions)])
       (define col (posn-col cell))
       (define row (posn-row cell))
       (define x (* col BOX-SIZE))
       (define y (* row BOX-SIZE))
       (println (format "putting pixel at (~a,~a)" x y))
       (send dc set-brush color 'solid)
       (send dc draw-rectangle x y BOX-SIZE BOX-SIZE)
       )
  (send target save-file "box.png" 'png))

(render points)