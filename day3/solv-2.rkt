#lang racket

(define MAX-BIT-INDEX 11)

(define (numbers_10->numbers_2 list-of-numbers)
  (map (lambda (n) (number->string n 2)) list-of-numbers))

(define (oxygen-rating list-of-numbers bit-index)
  (println (format "called oxygen-rating ~a ~a" list-of-numbers bit-index))
  (cond [(= (length list-of-numbers) 1) (first list-of-numbers)]
        [else (begin
                (define-values (ones zeros) (partition (lambda (n) (bitwise-bit-set? n bit-index))
                                                       list-of-numbers))
                (cond
                  [(= (length ones) (length zeros)) 
                   (begin
                     (println (format "recurse on ones: ~a" (numbers_10->numbers_2 ones)))
                     (oxygen-rating ones (sub1 bit-index)))]
                  [(> (length ones) (length zeros))
                   (begin
                     (println (format "recurse on ones: ~a" (numbers_10->numbers_2 ones)))
                     (oxygen-rating ones (sub1 bit-index)))]
                  [(< (length ones) (length zeros))
                   (begin
                     (println (format "recurse on zeros ~a" (numbers_10->numbers_2 zeros)))
                     (oxygen-rating zeros (sub1 bit-index)))]       

                  ))]
        ) 
  )

(define (co2-rating list-of-numbers bit-index)
  (println (format "called co2 ~a ~a" list-of-numbers bit-index))
  (cond [(= (length list-of-numbers) 1) (first list-of-numbers)]
        [else (begin
                (define-values (ones zeros) (partition (lambda (n) (bitwise-bit-set? n bit-index))
                                                       list-of-numbers))
                (cond
                  [(= (length ones) (length zeros)) 
                   (begin
                     (println (format "recurse on ones: ~a" (numbers_10->numbers_2 ones)))
                     (co2-rating zeros (sub1 bit-index)))]
                  [(< (length ones) (length zeros))
                   (begin
                     (println (format "recurse on ones: ~a" (numbers_10->numbers_2 ones)))
                     (co2-rating ones (sub1 bit-index)))]
                  [(> (length ones) (length zeros))
                   (begin
                     (println (format "recurse on zeros ~a" (numbers_10->numbers_2 zeros)))
                     (co2-rating zeros (sub1 bit-index)))]       

                  ))]
        ) 
  )

(define (solver)
  (define lines (port->lines (current-input-port)))
  (define numbers (map (lambda (l) (string->number l 2)) lines))
  (define oxygen (oxygen-rating numbers MAX-BIT-INDEX))
  (define co2 (co2-rating numbers MAX-BIT-INDEX))
  (* oxygen co2)
  )

(with-input-from-file "./input.txt"
                      solver
                      #:mode 'text)
