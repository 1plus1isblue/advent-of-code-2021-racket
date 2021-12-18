#lang racket

(require rackunit
         rackunit/text-ui)

;; test-input-1.txt returns too many paths. This script 
;; looks at the results and runs a verifier over each 
;; path to try and identify invalid paths.

(define verify-list
  '(
    ("start" "dc" "end")
    ("start" "dc" "LN" "dc" "end")
    ("start" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "dc" "LN" "dc" "HN" "end")
    ("start" "dc" "LN" "dc" "HN" "kj" "HN" "end")
    ("start" "dc" "kj" "dc" "end")
    ("start" "dc" "kj" "dc" "kj" "HN" "end")
    ("start" "dc" "kj" "dc" "HN" "end")
    ("start" "dc" "kj" "HN" "end")
    ("start" "dc" "kj" "HN" "dc" "end")
    ("start" "dc" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "dc" "kj" "HN" "dc" "HN" "end")
    ("start" "dc" "kj" "HN" "kj" "dc" "end")
    ("start" "dc" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "dc" "kj" "HN" "kj" "HN" "end")
    ("start" "dc" "kj" "sa" "kj" "dc" "end")
    ("start" "dc" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "dc" "kj" "sa" "kj" "HN" "end")
    ("start" "dc" "HN" "end")
    ("start" "dc" "HN" "dc" "end")
    ("start" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "dc" "HN" "dc" "HN" "end")
    ("start" "dc" "HN" "dc" "HN" "kj" "HN" "end")
    ("start" "dc" "HN" "kj" "dc" "end")
    ("start" "dc" "HN" "kj" "dc" "kj" "HN" "end")
    ("start" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "dc" "HN" "kj" "HN" "end")
    ("start" "dc" "HN" "kj" "HN" "dc" "end")
    ("start" "dc" "HN" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "dc" "HN" "kj" "HN" "dc" "HN" "end")
    ("start" "dc" "HN" "kj" "HN" "kj" "dc" "end")
    ("start" "dc" "HN" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "dc" "HN" "kj" "HN" "kj" "HN" "end")
    ("start" "dc" "HN" "kj" "sa" "kj" "dc" "end")
    ("start" "dc" "HN" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "dc" "HN" "kj" "sa" "kj" "HN" "end")
    ("start" "kj" "dc" "end")
    ("start" "kj" "dc" "LN" "dc" "end")
    ("start" "kj" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "kj" "dc" "LN" "dc" "HN" "end")
    ("start" "kj" "dc" "kj" "dc" "end")
    ("start" "kj" "dc" "kj" "dc" "HN" "end")
    ("start" "kj" "dc" "kj" "HN" "end")
    ("start" "kj" "dc" "HN" "end")
    ("start" "kj" "dc" "HN" "dc" "end")
    ("start" "kj" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "kj" "dc" "HN" "dc" "HN" "end")
    ("start" "kj" "dc" "HN" "kj" "dc" "end")
    ("start" "kj" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "kj" "dc" "HN" "kj" "HN" "end")
    ("start" "kj" "HN" "end")
    ("start" "kj" "HN" "dc" "end")
    ("start" "kj" "HN" "dc" "LN" "dc" "end")
    ("start" "kj" "HN" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "kj" "HN" "dc" "LN" "dc" "HN" "end")
    ("start" "kj" "HN" "dc" "kj" "dc" "end")
    ("start" "kj" "HN" "dc" "kj" "dc" "HN" "end")
    ("start" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "kj" "HN" "dc" "HN" "end")
    ("start" "kj" "HN" "dc" "HN" "dc" "end")
    ("start" "kj" "HN" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "kj" "HN" "dc" "HN" "dc" "HN" "end")
    ("start" "kj" "HN" "dc" "HN" "kj" "dc" "end")
    ("start" "kj" "HN" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "kj" "HN" "dc" "HN" "kj" "HN" "end")
    ("start" "kj" "HN" "kj" "dc" "end")
    ("start" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "kj" "HN" "kj" "HN" "end")
    ("start" "kj" "HN" "kj" "HN" "dc" "end")
    ("start" "kj" "HN" "kj" "HN" "dc" "HN" "end")
    ("start" "kj" "sa" "kj" "dc" "end")
    ("start" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "kj" "sa" "kj" "HN" "end")
    ("start" "kj" "sa" "kj" "HN" "dc" "end")
    ("start" "kj" "sa" "kj" "HN" "dc" "HN" "end")
    ("start" "HN" "end")
    ("start" "HN" "dc" "end")
    ("start" "HN" "dc" "LN" "dc" "end")
    ("start" "HN" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "LN" "dc" "HN" "end")
    ("start" "HN" "dc" "LN" "dc" "HN" "kj" "HN" "end")
    ("start" "HN" "dc" "kj" "dc" "end")
    ("start" "HN" "dc" "kj" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "kj" "HN" "dc" "end")
    ("start" "HN" "dc" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "kj" "HN" "dc" "HN" "end")
    ("start" "HN" "dc" "kj" "HN" "kj" "dc" "end")
    ("start" "HN" "dc" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "kj" "HN" "kj" "HN" "end")
    ("start" "HN" "dc" "kj" "sa" "kj" "dc" "end")
    ("start" "HN" "dc" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "kj" "sa" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "dc" "end")
    ("start" "HN" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "dc" "HN" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "dc" "end")
    ("start" "HN" "dc" "HN" "kj" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "dc" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "kj" "dc" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "HN" "kj" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "sa" "kj" "dc" "end")
    ("start" "HN" "dc" "HN" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "HN" "dc" "HN" "kj" "sa" "kj" "HN" "end")
    ("start" "HN" "kj" "dc" "end")
    ("start" "HN" "kj" "dc" "LN" "dc" "end")
    ("start" "HN" "kj" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "dc" "LN" "dc" "HN" "end")
    ("start" "HN" "kj" "dc" "kj" "dc" "end")
    ("start" "HN" "kj" "dc" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "dc" "HN" "dc" "end")
    ("start" "HN" "kj" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "dc" "HN" "dc" "HN" "end")
    ("start" "HN" "kj" "dc" "HN" "kj" "dc" "end")
    ("start" "HN" "kj" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "dc" "HN" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "end")
    ("start" "HN" "kj" "HN" "dc" "LN" "dc" "end")
    ("start" "HN" "kj" "HN" "dc" "LN" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "LN" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "kj" "dc" "end")
    ("start" "HN" "kj" "HN" "dc" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "dc" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "dc" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "kj" "dc" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "dc" "HN" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "kj" "dc" "end")
    ("start" "HN" "kj" "HN" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "HN" "kj" "HN" "end")
    ("start" "HN" "kj" "HN" "kj" "HN" "dc" "end")
    ("start" "HN" "kj" "HN" "kj" "HN" "dc" "HN" "end")
    ("start" "HN" "kj" "sa" "kj" "dc" "end")
    ("start" "HN" "kj" "sa" "kj" "dc" "HN" "end")
    ("start" "HN" "kj" "sa" "kj" "HN" "end")
    ("start" "HN" "kj" "sa" "kj" "HN" "dc" "end")
    ("start" "HN" "kj" "sa" "kj" "HN" "dc" "HN" "end")
    )
  )

(define (start-only-at-beginning? path)
  (and (string=? "start" (car path))
       (= 1 (count (lambda (el) (string=? el "start")) path))))

(define (end-only-at-end? path)
  (and (string=? "end" (car (reverse path)))
       (= 1 (count (lambda (el) (string=? el "end")) path))))

(define (is-small? node)
  (and (andmap char-lower-case? (string->list node))
       (not (string=? node "start"))
       (not (string=? node "end"))))

;; List<String> -> Map<String, Number>
(define (count-occurences path)
  (define grouped-by-name (group-by (lambda (name) name) path))
  (define counts (map (lambda (a-list) (cons (car a-list) (length a-list))) grouped-by-name))
  (make-hash counts))

;; Says if there exists a small node that was visited more than twice
;; Map<String, Number> Number -> Boolean
(define (a-small-greater-than? counts threshold)
  (sequence-ormap (lambda (k+v) (and (is-small? (car k+v))
                                     (> (cdr k+v) threshold)))
                  (in-hash-pairs counts))
  )

(define (only-1-small-count-greater-than-1? counts)
  (<= (length (sequence->list (sequence-filter (lambda (k+v) (>= (cdr k+v) 2))
                                               (sequence-filter (lambda (k+v) (is-small? (car k+v)))
                                                                (in-hash-pairs counts)))))
      1))

(define (valid-path? path)
  (define visit-counts (count-occurences path))
  (define result (and (only-1-small-count-greater-than-1? visit-counts)
       (not (a-small-greater-than? visit-counts 2))
       (start-only-at-beginning? path)
       (end-only-at-end? path)))
  result)

;; Identify invalid paths 
;; List<List<String>> -> List<List<String>>
(define (find-invalid-paths paths)
  (filter-not valid-path? paths))

(define tests
  (test-suite
    "tests"
    (test-case "count-occurences with a small node having more than 2"
               (define the-path '("ab" "NA" "ab" "CD" "ab"))
               (define expect (make-hash (list (cons "ab" 3) (cons "NA" 1) (cons "CD" 1))))
               (check-equal? (count-occurences the-path) expect))

    (test-case "a-small-greater-than? with a small node having more than 2 returns true"
               (define the-path '("start" "bs" "N" "th" "th" "bs" "N" "bs" "end"))
               (define occurences (count-occurences the-path))
               (check-equal? (a-small-greater-than? occurences 2) #t))

    (test-case "a-small-greater-than? with no a small node having more than 2 returns false"
               (define the-path '("start" "bs" "N" "th" "th" "N" "end"))

               (define occurences (count-occurences the-path))
               (check-equal? (a-small-greater-than? occurences 2) #f))

    (test-case "a-small-greater-than? with a large node having more than 2 and no small nodes with more than 2 returns false"
               (define the-path '("start" "bs" "N" "th" "bs" "N" "N" "end"))
               (define occurences (count-occurences the-path))
               (check-equal? (a-small-greater-than? occurences 2) #f))

    (test-case "only-1-small-count-greater-than-1? if 1 small node has 2 then all other small nodes exactly 1 -> true"
               (define the-path '("start" "bs" "N" "th" "bs" "N" "N" "end"))
               (define occurences (count-occurences the-path))
               (check-equal? (only-1-small-count-greater-than-1? occurences)
                             #t)
               )

    (test-case "only-1-small-count-greater-than-1? if 2 small nodes have 2 -> false"
               (define the-path '("start" "bs" "N" "th" "bs" "th" "N" "N" "end"))
               (define occurences (count-occurences the-path))
               (check-equal? (only-1-small-count-greater-than-1? occurences)
                             #f))

    (test-case "valid-path? valid paths -> true"
               (define the-paths '(
                                   ("start" "bs" "N" "end")
                                   ("start" "end")
                                   ("start" "bs" "N" "bs" "end")
                                   ("start" "bs" "end")
                                   ("start" "bs" "bs" "end")
                                   ("start" "N" "end")
                                   ))
               (for ([a-path (in-list the-paths)])
                    (check-equal? (valid-path? a-path)
                                  #t)
                    ))

    (test-case "valid-path? invalid paths -> false"
               (define the-paths '(
                                   ("start" "start")
                                   ("start" "end" "end")
                                   ("end" "start")
                                   ("start" "bs" "bs"  "bs" "end")
                                   ("start" "bs" "bs" "d" "d" "end")
                                   ))
               (for ([a-path (in-list the-paths)])
                    (check-equal? (valid-path? a-path)
                                  #f)))

    (test-case "find-invalid-paths returns only invalid paths"
               (define the-paths '(
                                   ("start" "end") ; valid
                                   ("start" "bs" "bs"  "bs" "end") ; invalid
                                   ("start" "bs" "bs" "end") ; valid
                                   ("start" "bs" "bs" "d" "d" "end") ; invalid
                                   ("start" "bs" "N" "bs" "end") ; valid
                                   ))
               (define expected '(
                                   ("start" "bs" "bs"  "bs" "end") ; invalid
                                   ("start" "bs" "bs" "d" "d" "end") ; invalid
                                  ))
               (check-equal? (find-invalid-paths the-paths)
                             expected)
               )

    ;; closes test-suite
    ))

(run-tests tests)

(define bad-output-paths (find-invalid-paths verify-list))
(println (format "found ~a bad paths" (length bad-output-paths)))
(for ([bad-path (in-list bad-output-paths)])
     (println bad-path))

