#lang racket
(require math)

;; Note: read Matrix[C x R] as a matrix with C columns and R rows

(define DAYS 256)

;; List<PositiveInteger> -> Matrix[1x9]
(define (initialize-start-vector list-of-integers)
  (define bucket-counts (group-by identity list-of-integers))

  (define existing-key-pairs (map (lambda (c) (cons (car c) (length c))) bucket-counts))
  (define existing-keys (list->set (map car existing-key-pairs)))
  (define expected-keys (set 0 1 2 3 4 5 6 7 8))

  ;; Find missing keys
  (define missing-key-pairs (map (lambda (key) (cons key 0))
                            (set->list (set-subtract expected-keys existing-keys))))

  ;; Construct intermediate snapshot of key-value mapping 
  (define all-key-pairs (append existing-key-pairs missing-key-pairs))
  (define sorted-key-pairs (sort all-key-pairs (lambda (n1 n2) (< (car n1) (car n2)))))

  ;; Strip out keys and make flat 
  (define flattened-counts (flatten (map cdr sorted-key-pairs)))

  ;; Convert into a vector. Ignore the term "matrix". This allows us to use matrix operations
  (list->matrix 9 1 flattened-counts))

(define (solver)
  (define current-fish-list (map string->number (string-split (read-line (current-input-port)) ",")))
  (define current-fish-vector (initialize-start-vector current-fish-list))
  (define growth-matrix (matrix [;0 1 2 3 4 5 6 7 8  
                                 [0 1 0 0 0 0 0 0 0] ; 0 
                                 [0 0 1 0 0 0 0 0 0] ; 1
                                 [0 0 0 1 0 0 0 0 0] ; 2
                                 [0 0 0 0 1 0 0 0 0] ; 3
                                 [0 0 0 0 0 1 0 0 0] ; 4
                                 [0 0 0 0 0 0 1 0 0] ; 5
                                 [1 0 0 0 0 0 0 1 0] ; 6
                                 [0 0 0 0 0 0 0 0 1] ; 7
                                 [1 0 0 0 0 0 0 0 0] ; 8
                                 ]))
  (define growth-matrix-after-n-days (matrix-expt growth-matrix DAYS))
  (apply + (matrix->list (matrix* growth-matrix-after-n-days current-fish-vector)))
 )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
