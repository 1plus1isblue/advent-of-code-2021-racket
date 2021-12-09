#lang racket

(define (initial-config words)
  (let* ([word-length (lambda (len) (lambda (word) (= len (string-length word))))]
         [word->set (compose list->set string->list)]
         [one (word->set (findf (word-length 2) words))]
         [four (word->set (findf (word-length 4) words))]
         [seven (word->set (findf (word-length 3) words))]
         [eight (word->set (findf (word-length 7) words))]
         [p0 (set-subtract seven one)]
         [p1 (set-subtract four one)]
         [p2 one]
         [p3 (set-subtract four one)]
         [p4 (set-subtract (set-subtract eight seven) four)]
         [p5 one]
         [p6 (set-subtract (set-subtract eight seven) four)]
         )
    (list->vector `(,p0 ,p1 ,p2 ,p3 ,p4 ,p5 ,p6))))

(define (numbers-using-segments)
  (vector (set 0 2 3 5 6 7 8 9)
          (set 0 4 5 6 8 9)
          (set 0 1 2 3 4 7 8 9)
          (set 2 3 4 5 6 8 9)
          (set 0 2 6 8)
          (set 0 1 3 4 5 6 7 8 9)
          (set 0 2 3 5 6 8 9))
  )

;; Indicates which segments are used for displaying a number
(define (segments-per-number)
  (make-hash (list (cons 0 (set 0 1 2 4 5 6))
                   (cons 1 (set 2 5))
                   (cons 2 (set 0 2 3 4 6))
                   (cons 3 (set 0 2 3 5 6))
                   (cons 4 (set 1 2 3 5))
                   (cons 5 (set 0 1 3 5 6))
                   (cons 6 (set 0 1 3 4 5 6))
                   (cons 7 (set 0 2 5))
                   (cons 8 (set 0 1 2 3 4 5 6))
                   (cons 9 (set 0 1 2 3 5 6)))))

;; Tries potential assignments for segment i.
;; Returns pair (assignment for i, not assignment for i)
(define (try-assignment check-set segment-assignments i)
  (define p-options (vector-ref segment-assignments i))
  (define option-a (set-first p-options))
  (define option-b (set-first (set-remove p-options option-a)))
  (if (works? check-set option-a)
    (values option-a option-b)
    (values option-b option-a)))

;; Each set in check-set should contain the assignment
(define (works? check-set assignment)
  (and (set-member? (first check-set) assignment)
       (set-member? (second check-set) assignment)
       (set-member? (third check-set) assignment)))

(define (find-segments unmapped-numbers)
  (define segment-assignments (initial-config unmapped-numbers))
  (define check-set (map list->set
                         (filter (lambda (l) (= (length l) 6))
                                 (map string->list unmapped-numbers)))) ;; only sets with length == 6
  (define-values (p5 p2) (try-assignment check-set segment-assignments 5))
  (define-values (p1 p3) (try-assignment check-set segment-assignments 1))
  (define-values (p6 p4) (try-assignment check-set segment-assignments 6))
  (make-hash (list (cons 0 (set-first (vector-ref segment-assignments 0)))
        (cons 1 p1)
        (cons 2 p2)
        (cons 3 p3)
        (cons 4 p4)
        (cons 5 p5)
        (cons 6 p6))))

;; Set<Numbers> Map<Number, Character> -> Set<Character>
(define (segments->letters segments-for-a-number segment-to-letters)
  (for/set ([segment (in-set segments-for-a-number)])
           (hash-ref segment-to-letters segment)))

;; Segment -> Character map gets converted into a mapping of segment combinations to specific numbers
;; Map<Number, Character> -> Map<Set<Character>, Number>
(define (segment->mapping segment-mapping)
  (make-hash (for/list ([number (in-range 0 10)])
            (cons (segments->letters (hash-ref (segments-per-number) number) segment-mapping)
                  number))))

(define (lookup-number mapping character-set)
  (hash-ref mapping character-set)
  )

(define (to-number numbers-mapping unmapped-outputs)
  (define output-sets (map (compose list->set string->list) unmapped-outputs))
  (define output-numbers (map (curry lookup-number numbers-mapping) output-sets))
  (+ (* 1000 (first output-numbers))
     (* 100 (second output-numbers))
     (* 10 (third output-numbers))
     (* 1 (fourth output-numbers))))


(define (solver)
  (define lines (map (lambda (l) (map string-split l))
                     (map (lambda (line) (string-split line "|"))
                          (port->lines (current-input-port)))))
  (for/sum ([line (in-list lines)])
           (define unmapped-segments (car line))
           (define unmapped-outputs (car (cdr line)))
           (define letter-to-segment (find-segments unmapped-segments))
           ;(println letter-to-segment)
           (define numbers-mapping (segment->mapping letter-to-segment))
           ;(println numbers-mapping)
           (to-number numbers-mapping unmapped-outputs))
  )


(define filename (vector-ref (current-command-line-arguments) 0))

(with-input-from-file filename 
                      solver
                      #:mode 'text)
