#lang racket
(require "../helpers.rkt"
         rackunit
         rackunit/text-ui)

(define (list->pair l)
  (cons (car l) (car (cdr l))))

(define (string->rule s)
  (list->pair (map string-trim (string-split s "->"))))

(define (expand-polymer existing mapping)
  (define first-char (string-ref existing 0))
  (list->string (flatten (cons first-char (for/list ([start-idx (in-range 0 (sub1 (string-length existing)))])
                                                    (expand-pair (substring existing start-idx (+ start-idx 2)) mapping))))))

;; Returns middle and remaining. Drops the first string.
(define (expand-pair two-letters mapping)
  (define first-half (substring two-letters 0 1))
  (define second-half (substring two-letters 1 2))
  (string->list (string-append (hash-ref mapping two-letters) second-half)))

(define (expand-iters start mapping iters)
  (define current start)
  (for ([i (in-range iters)])
       (println (format "iteration ~a" i))
       (set! current (expand-polymer current mapping)))
  current)

(define (count-letters s)
  (map (lambda (l) (cons (car l) (length l))) (group-by identity (string->list s))))

(define (max-count s)
  (first (sort (count-letters s)
        (lambda (kv1 kv2)
          (> (cdr kv1)
             (cdr kv2))))))

(define (min-count s)
  (first (sort (count-letters s)
        (lambda (kv1 kv2)
          (< (cdr kv1)
             (cdr kv2))))))

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define start (first lines))
  (define rules (make-hash (map string->rule (drop lines 2))))
  (define final-string (expand-iters start rules 40))
  (- (cdr (max-count final-string))
     (cdr (min-count final-string)))
  )

(setup-and-run solver)

(define tests
  (test-suite
    "tests"

    (test-case "expand single pair inserts in middle"
               (define mapping (make-hash (list (cons "NN" "B"))))
               (define start "NN")
               (define expected "NBN")
               (check-equal? (expand-polymer start mapping)
                             expected))

    (test-case "expand-pair returns middle and last"
               (define mapping (make-hash (list (cons "NN" "B"))))
               (define pair-of-letters "NN")
               (define expected (string->list "BN"))
               (check-equal? (expand-pair pair-of-letters mapping)
                             expected)

               )

    (test-case "expand two pairs inserts between both"
               (define mapping (make-hash (list (cons "NN" "B")
                                                (cons "NB" "C"))))
               (define start "NNB")
               (define expected "NBNCB")
               (check-equal? (expand-polymer start mapping)
                             expected))

    (test-case "example case: NNCB"
               (define mapping (make-hash (list (cons "NN" "C")
                                                (cons "NC" "B")
                                                (cons "CB" "H"))))
               (define start "NNCB")
               (define expected "NCNBCHB")

               (check-equal? (expand-polymer start mapping)
                             expected))

    (test-case "two iterations of NN -> NCN -> NBCCN"
               (define mapping (make-hash (list (cons "NN" "C")
                                                (cons "NC" "B")
                                                (cons "CN" "C"))))
               (define start "NN")
               (define expected "NBCCN")
               (check-equal? (expand-polymer (expand-polymer start mapping) mapping)
                             expected))

    (test-case "run multiple iterations"
               (define mapping (make-hash (list (cons "NN" "C")
                                                (cons "NC" "B")
                                                (cons "CN" "C"))))
               (define start "NN")
               (define expected "NBCCN")
               (check-equal? (expand-iters start mapping 2)
                             expected)
               )

    (test-case "test-input"
               (define mapping (make-hash (list
                                            (cons "CH" "B")
                                            (cons "HH" "N")
                                            (cons "CB" "H")
                                            (cons "NH" "C")
                                            (cons "HB" "C")
                                            (cons "HC" "B")
                                            (cons "HN" "C")
                                            (cons "NN" "C")
                                            (cons "BH" "H")
                                            (cons "NC" "B")
                                            (cons "NB" "B")
                                            (cons "BN" "B")
                                            (cons "BB" "N")
                                            (cons "BC" "B")
                                            (cons "CC" "N")
                                            (cons "CN" "C")
                                            )))

               (define start "NNCB")
               (define expected-1 "NCNBCHB")
               (define expected-2 "NBCCNBBBCBHCB")
               (define expected-3 "NBBBCNCCNBBNBNBBCHBHHBCHB")
               (define expected-4 "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
               (check-equal? (expand-iters start mapping 1)
                             expected-1)

               (check-equal? (expand-iters start mapping 2)
                             expected-2)

               (check-equal? (expand-iters start mapping 3)
                             expected-3)

               (check-equal? (expand-iters start mapping 4)
                             expected-4)
               )

    (test-case "counts gets mapping of letters to counts"
               (define a-string "CCNC")
               (check-equal? (count-letters a-string)
                             (list (cons #\C 3)
                                   (cons #\N 1))))

    (test-case "get max count letter"
               (define a-string "CCNC")
               (check-equal? (max-count a-string)
                             (cons #\C 3))
               )

    (test-case "get min count letter"
               (define a-string "CCNC")
               (check-equal? (min-count a-string)
                             (cons #\N 1))
               )

    ;; closes test-suite
    ))

(run-tests tests)
