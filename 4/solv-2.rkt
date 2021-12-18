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

(define (score board)
  (for/sum ([row (in-vector board)])
           (for/sum ([cell (in-vector row)])
                    (if (cell-marked cell)
                      0
                      (cell-value cell)))))

(define (all-boards-won? boards)
  (define ret-val (andmap bingo? boards))
  ret-val
  )

(define (solver)
  (define moves (map string->number (string-split (read-line (current-input-port)) ",")))
  (define board-lines (filter-not empty?
                                  (map (lambda (l) (string-split l))
                                       (port->lines (current-input-port)))))
  (define the-boards (assemble-boards board-lines))
  (define last-move #f)
  (define last-board #f)
  (for ([move (in-list moves)])
       #:break (all-boards-won? the-boards)
       (println move)
       (mark-boards the-boards move)
       (set! last-move move)
       (if (= 1 (length the-boards)) ;; keep track of remaining boards
         void ;; keep last board to finish
         (set! the-boards (filter-not bingo? the-boards))) ;; filter out winning boards
       )
  (println "DONE")
  (println last-move)
  (println the-boards)
  (* last-move (score (first the-boards))) ;; the-boards only contains 1 element
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
