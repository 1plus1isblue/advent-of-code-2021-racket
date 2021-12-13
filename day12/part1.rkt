#lang racket
(require "../helpers.rkt")

(define (add-pair graph keypair)
  (define key (car keypair))
  (define value (car (cdr keypair)))
  (if (hash-has-key? graph key)
    (hash-set! graph key (set-add (hash-ref graph key) value))
    (begin (hash-set! graph key (set))
           (hash-set! graph key (set-add (hash-ref graph key) value))))
  )

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

;; Cases:
;; - At non-end node with nothing valid adjacent
;; - Reached "end"
;; - Non-end with valid adjacent 
;; Node Graph Set<Nodes> -> Integer
(define (sum-paths current the-graph small-visited)
  ;;(println (format "sum-paths ~a ~a ~a" current the-graph small-visited))
  (define all-adjacent (hash-ref the-graph current))
  (define valid-adjacent (set-subtract all-adjacent small-visited))
  ;;(println (format "valid-adjacent ~a" valid-adjacent))
  (define updated-small-visited (if (is-small? current) (set-add small-visited current) small-visited))
  (cond [(and (set-empty? valid-adjacent)
              (not (string=? "end" current))) 0] ;; Cannot continue and not at "end"
        [(string=? "end" current) 1] ;; reached "end"
        [else (begin 
                (+ (for/sum ([next (in-set valid-adjacent)])
                            ;;(println (format "next ~a, updated-small-visited ~a" next updated-small-visited))
                            (sum-paths next the-graph updated-small-visited))))]))

(define (solver)
  (define the-graph (build-bi-graph))
  (sum-paths "start" the-graph (set "start")))

(setup-and-run solver)
