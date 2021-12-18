#lang racket

(define-struct cell (value marked) #:mutable #:transparent)
;; Board is Vector<Vector<Cell>>

;; Converts a list of 25 numbers to a board
;; List<List<Number>> -> Board
(define (to-board numbers)
  (define cell-builder (lambda (n) (make-cell (string->number n) #f)))
  (for/vector ([row (in-list numbers)])
              (for/vector ([element (in-list row)])
                          (cell-builder element))))

;; Takes a list of a list of strings, each representing a line in a board.
;; Every 5 lines makes a board.
;; List<List<String>> -> List<Board>
(define (assemble-boards lines)
  (cond [(empty? lines) null]
        [else (cons (to-board (take lines 5))
                    (assemble-boards (drop lines 5)))]))

(define (mark-board board move)
  (for ([row  (in-vector board)])
       (for ([a-cell (in-vector row)])
            (if (= (cell-value a-cell) move)
              (set-cell-marked! a-cell #t)
              void))))

(define (mark-boards boards move)
  (for ([board (in-list boards)])
       (mark-board board move)))

;; Says if bingo is in a column
(define (in-column? board col-number) 
  (for/and ([row (in-vector board)])
           (cell-marked (vector-ref row col-number))))

(define (in-row? board row-num)
  (andmap cell-marked (vector->list (vector-ref board row-num))))

(define (bingo? board)
  (or (for/or ([col (in-range 5)])
              (in-column? board col))
      (for/or ([row (in-range 5)])
              (in-row? board row))))

(define (board-has-bingo? boards)
  (define ret-val (ormap bingo? boards))
  ret-val)

(define (score boards)
  (define winning-board (findf bingo? boards))
  (for/sum ([row (in-vector winning-board)])
           (for/sum ([cell (in-vector row)])
                    (if (cell-marked cell)
                      0
                      (cell-value cell)))))

(define (solver)
  (define moves (map string->number (string-split (read-line (current-input-port)) ",")))
  (define board-lines (filter-not empty?
                                  (map (lambda (l) (string-split l))
                                       (port->lines (current-input-port)))))
  (define the-boards (assemble-boards board-lines))
  (define last-move #f)
  (for ([move (in-list moves)])
       #:break (board-has-bingo? the-boards)
       (mark-boards the-boards move)
       (set! last-move move))
  (* (score the-boards) last-move)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
