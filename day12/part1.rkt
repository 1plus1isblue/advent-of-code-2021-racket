#lang racket
(require "../helpers.rkt")

(define debug-on? #f)

(define (add-pair graph keypair)
  (define key (car keypair))
  (define value (car (cdr keypair)))
  (if (hash-has-key? graph key)
    (hash-set! graph key (set-add (hash-ref graph key) value))
    (begin (hash-set! graph key (set))
           (hash-set! graph key (set-add (hash-ref graph key) value)))))

(define (build-bi-graph)
  (define lines (port->lines (current-input-port)))
  (define graph (make-hash))
  (for ([line (in-list lines)])
            (define nodes (string-split line "-"))
            (add-pair graph nodes)
            (add-pair graph (reverse nodes))
            )
  (println (format "the-graph: ~a" graph))
  graph)

(define (is-small? node-name)
  (andmap char-lower-case? (string->list node-name)))

;; Map<String,Number> -> Boolean
(define (node-visited-twice? nodes visit-count)
  (sequence-ormap (lambda (p) (and (is-small? (car p))
                                   (= 2 (cdr p))))
                  (in-hash-pairs visit-count)))

;; Determines valid adjacent nodes to visit
;; Cases per node:
;; * "start" -> not valid
;; * no small nodes visited more than once and considering a small node -> valid
;; * there exists a small node visisted more than once and considering a small node visited 0 times -> valid
;; * there exists a small node visisted more than once and considering a small node visited 1 times -> invalid
;; * big node -> valid
;; Set<String> Map<String, Number> -> Set<String>
(define (get-valid-adjacent all-adjacent visit-count)
  (define a-small-node-visited-twice? (node-visited-twice? all-adjacent visit-count))
  (define valid-adjacent
    (for/set ([neighbor (in-set all-adjacent)])
             (define node-visit-count (hash-ref visit-count neighbor))
             (define small-node? (is-small? neighbor))
             (cond [(string=? "start" neighbor) null]
                   [(and small-node? a-small-node-visited-twice? (= node-visit-count 0)) neighbor] ;; small nodes can only be visited once
                   [(and small-node? a-small-node-visited-twice? (> node-visit-count 0)) null] ;; small nodes can only be visited once
                   [(and small-node? (not a-small-node-visited-twice?) (= node-visit-count 0)) neighbor] ;; small nodes can only be visited once
                   [(and small-node? (not a-small-node-visited-twice?) (> node-visit-count 0)) neighbor] ;; small nodes can only be visited once
                   [else neighbor]) ;; large node
             ))
  (set-remove valid-adjacent null)) ;; 

;; Cases:
;; - At non-end node with nothing valid adjacent
;; - Reached "end"
;; - Non-end with valid adjacent 
;; Node Graph Map<Node, Number> -> Integer
(define (sum-paths current the-graph visit-count path)
  (set! debug-on? (equal? (reverse path) '("start" "A" "c" "A" "c" "A" "b" "d")))
  (define valid-adjacent (get-valid-adjacent (hash-ref the-graph current) visit-count))
  (define updated-visit-count (hash-set visit-count current (add1 (hash-ref visit-count current))))
  (cond [(and (set-empty? valid-adjacent)
              (not (string=? "end" current))) 0] ;; Cannot continue and not at "end"
        [(string=? "end" current) 1] ;; reached "end"
        [else (begin 
                (+ (for/sum ([next (in-set valid-adjacent)])
                            (sum-paths next the-graph updated-visit-count (cons next path)))))]))

;; Initializes map of number of times a node has been visited
;; List<String> -> Map<String, Number>
(define (build-visit-count nodes)
  (for/hash ([key (in-list nodes)])
            (values key 0)))

(define (solver)
  (define the-graph (build-bi-graph))
  (sum-paths "start" the-graph (build-visit-count (hash-keys the-graph)) '("start")))

(setup-and-run solver)
