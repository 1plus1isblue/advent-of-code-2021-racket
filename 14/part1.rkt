#lang racket
(require "../helpers.rkt"
         racket/hash
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
       (set! current (expand-polymer current mapping)))
  current)

;; String -> List<Pair<Character, Number>>
(define (count-letters s)
  (make-immutable-hash (map (lambda (l) (cons (car l) (length l))) (group-by identity (map string (string->list s))))))

;; Find value of largest number in a map
;; Map<String,Number> -> Number
(define (max-bucket-count h)
  (apply max (hash-values h)))

(define (min-bucket-count h)
  (apply min (hash-values h)))

(define (max-count s)
  (first (sort (hash->list (count-letters s))
        (lambda (kv1 kv2)
          (> (cdr kv1)
             (cdr kv2))))))

(define (min-count s)
  (first (sort (hash->list (count-letters s))
        (lambda (kv1 kv2)
          (< (cdr kv1)
             (cdr kv2))))))

;; Produces a function that counts letters in resulting string after expanding polymer-pair for _n_ iterations
;; mapping provides rules for how to expand polymer-pair. Assume that polymer exists
;; String Number Mapping -> (String Number . -> .  Map<String, Number>)
(define (build-letter-count mapping)
  (define results-lookup (make-hash))
  (define (fn polymer iters)
    (define insertion-letter (hash-ref mapping polymer))
    (define first-letter (substring polymer 0 1))
    (define second-letter (substring polymer 1 2))
    (cond [(hash-has-key? results-lookup `(,polymer ,iters)) (hash-ref results-lookup `(,polymer ,iters))]
          [(= iters 0) (begin 
                         (define res (count-letters polymer))
                         (hash-set! results-lookup `(,polymer ,iters) res)
                         res)]
          [else (begin 
                  (define left-pair (string-append first-letter insertion-letter))
                  (define right-pair (string-append insertion-letter second-letter))
                  ;(println (format "recurse on left ~a and right ~a" left-pair right-pair))
                  (define temp (hash-union (fn left-pair (sub1 iters))
                                           (fn right-pair (sub1 iters))
                                           #:combine/key (lambda (k v1 v2) (+ v1 v2))))
                  (define result (hash-set temp insertion-letter (sub1 (hash-ref temp insertion-letter))))
                  ;(println (format "recursed on left ~a and right ~a with result ~a" left-pair right-pair result))
                  (hash-set! results-lookup `(,polymer ,iters) result)
                  result
                  )])
    )
  fn
  )

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define start (first lines))
  (define rules (make-hash (map string->rule (drop lines 2))))
  (define the-fn (build-letter-count rules))
  (define first-pair (substring start 0 2))
  (define second-pair (substring start 1 3))
  (define third-pair (substring start 2 4))

  (define ITERS 40)

  (define overcounted-total (time (hash-union (the-fn first-pair ITERS)
                                              (the-fn second-pair ITERS)
                                              (the-fn third-pair ITERS)
                                              #:combine/key (lambda (k v1 v2) (+ v1 v2)))))

  (define middle-counts (count-letters (substring start 1 (sub1 (string-length start)))))
  (define middle-counts-removed (hash-union overcounted-total middle-counts
                                            #:combine/key (lambda (k v1 v2) (- v1 v2))))
  (println middle-counts-removed)

  (println (format "result: ~a" (- (max-bucket-count middle-counts-removed)
                                   (min-bucket-count middle-counts-removed))))
  #t

  )

(define tests
  (test-suite
    "tests"

    (test-case "function that returns letter counts")

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
                             (make-immutable-hash (list (cons "C" 3)
                                                        (cons "N" 1)))))

    (test-case "get max count letter"
               (define a-string "CCNC")
               (check-equal? (max-count a-string)
                             (cons "C" 3))
               )

    (test-case "build-letter-count produces a function"
               (define the-fn (build-letter-count (hash "AC" "B")))
               (check-equal? (procedure? the-fn) #t))

    (test-case "build-letter-count fn returns count for base case"
               (define the-fn (build-letter-count (hash "AC" "B")))
               (check-match (the-fn "AC" 0) (hash-table ("A" 1) ("C" 1)))
               ) 

    (test-case "build-letter-count fn returns count for base+1 case"
               (define the-fn (build-letter-count (hash "AC" "B" "AB" "C" "BC" "A")))
               (check-match (the-fn "AC" 1) (hash-table ("A" 1) ("B" 1) ("C" 1)))
               )

    (test-case "build-letter-count fn returns count for base+2 case: AC 2 -> ABC 1 -> ACBAC 0"
               (define the-fn (build-letter-count (hash "AC" "B" "AB" "C" "BC" "A" "CB" "A" "CA" "B" "BA" "C")))
               (check-match (the-fn "AC" 2) (hash-table ("A" 2) ("B" 1) ("C" 2)))
               )



    (test-case "get min count letter"
               (define a-string "CCNC")
               (check-equal? (min-count a-string)
                             (cons "N" 1))
               )

    (test-case "merge with hash-union. TODO this can be rewritten away after implementation"
               (define h1 (make-hash (list (cons "A" 1)
                                           (cons "B" 3))))
               (define h2 (make-hash (list (cons "A" 1)
                                           (cons "C" 4))))
               (check-match (hash-union (make-immutable-hash (hash->list h1))
                                        (make-immutable-hash (hash->list h2))
                                        #:combine/key(lambda (k v1 v2) (+ v1 v2)))
                            (hash-table ("A" 2) ("B" 3) ("C" 4))))

    (test-case "hash-union more than 2 hashes"
               (check-match (hash-union (hash "A" 1 "B" 1)
                                        (hash "A" 1 "B" 1)
                                        (hash "B" 1 "C" 1)
                                        #:combine/key (lambda (k v1 v2) (+ v1 v2)))
                            (hash-table ("A" 2) ("B" 3) ("C" 1))))

    (test-case "letter-count for NN with test-input-1.txt should give correct letter count with various iterations"
               (define rules
                 (make-hash (map string->rule '("CH -> B"
                                                "HH -> N"
                                                "CB -> H"
                                                "NH -> C"
                                                "HB -> C"
                                                "HC -> B"
                                                "HN -> C"
                                                "NN -> C"
                                                "BH -> H"
                                                "NC -> B"
                                                "NB -> B"
                                                "BN -> B"
                                                "BB -> N"
                                                "BC -> B"
                                                "CC -> N"
                                                "CN -> C")))
                 )
               (define the-fn (build-letter-count rules))
               (check-match (the-fn "NN" 1)
                            (hash-table ("N" 2) ("C" 1)))

               (check-match (the-fn "NN" 2) ;; NN -> NCN -> NBCCN
                            (hash-table ("N" 2) ("C" 2) ("B" 1)))

               (check-match (the-fn "NN" 3) ;; NBCCN -> NBBBCNCCN
                            (hash-table ("N" 3) ("B" 3) ("C" 3))
                            )
               )

    (test-case "hash-union when removing redundant counts"
               (define h1 (hash "A" 1 "B" 2 "C" 3))
               (define h2 (hash "A" 1 "B" 1))
               (check-match (hash-union h1 h2
                                        #:combine/key (lambda (k v1 v2) (- v1 v2)))
                            (hash-table ("A" 0) ("B" 1) ("C" 3)))
               )
    (test-case "get largest bucket size"
               (define h (hash "A" 1 "B" 2))
               (check-equal? (max-bucket-count h)
                             2))

    ;; closes test-suite
    ))

(run-tests tests)

(println "Tests passed. Running main program with input")

(setup-and-run solver)

(println (format "step 0 ~a: ~a"  "NNCB" (count-letters "NNCB")))
(println (format "step 1 ~a: ~a"  "NCNBCHB" (count-letters "NCNBCHB")))
(println (format "step 2 ~a: ~a"  "NBCCNBBBCBHCB" (count-letters "NBCCNBBBCBHCB")))
(println (format "step 3 ~a: ~a"  "NBBBCNCCNBBNBNBBCHBHHBCHB" (count-letters "NBBBCNCCNBBNBNBBCHBHHBCHB")))
(println (format "step 4 ~a: ~a"  "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" (count-letters "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")))
