#lang typed/racket
(require "../helpers.rkt"
         typed/rackunit
         typed/rackunit/text-ui)

(define-type BinaryString String)
(define-type HexString String)
(define-type Index Integer)

(define-struct Packet ([version : Number]
                       [type : Number])
                       #:transparent)

(define-struct (LiteralPacket Packet)
               ([value : Number]
                [last-bit-pos : Index])
               #:transparent)

(define-struct (OperatorPacket Packet)
               ([length-type : Number]
                [sub-length : Number]
                [subpackets : (Listof Packet)])
                #:transparent)

(define (hex-to-binary s)
  (hash-ref (hash "0" "0000"
                  "1" "0001"
                  "2" "0010"
                  "3" "0011"
                  "4" "0100"
                  "5" "0101"
                  "6" "0110"
                  "7" "0111"
                  "8" "1000"
                  "9" "1001"
                  "A" "1010"
                  "B" "1011"
                  "C" "1100"
                  "D" "1101"
                  "E" "1110"
                  "F" "1111")
            s))

(: last-group? (-> BinaryString Index Boolean))
(define (last-group? s offset)
  (define the-bit (substring s offset (add1 offset)))
  (string=? "0" the-bit))

(: get-last-bit-index (-> BinaryString Index Index))
(define (get-last-bit-index s value-start)
  (define OFFSET-FROM-START-OF-LAST-BLOCK 4)
  (define GROUP-SIZE 5)
  (do ([offset value-start (+ GROUP-SIZE offset)])
    ((last-group? s offset) (+ offset OFFSET-FROM-START-OF-LAST-BLOCK))))

(: string->packet (-> String (Struct LiteralPacket)))
(define (string->packet s) 
  (let* ([binary-form : BinaryString (hex-string->binary-string s)]
         [version : Number (or-fail (string->number (binary-ref-most-sig binary-form 0 3) 2))]
         [type : Number (or-fail (string->number (binary-ref-most-sig binary-form 3 6) 2))])
    (cond [(= type 4) (make-LiteralPacket version type 0 (get-last-bit-index binary-form 6))]
          [else (error "Not implemented")])
    ))

;; Converts a string representation of a hex to a binary representation of it.
;; Example "BD" -> "10111101"
(: hex-string->binary-string (-> HexString BinaryString))
(define (hex-string->binary-string s)
  (foldr string-append
         ""
         (map hex-to-binary
              (map string (string->list s)))))

;; Gets [start, end] binary digits from left to right of the BinaryString
(: binary-ref-most-sig (-> BinaryString Index Index BinaryString))
(define (binary-ref-most-sig [s : BinaryString]
                        [start : Index]
                        [end : Index])
  (substring s start end))

(: or-fail (-> (U Complex False) Number))
(define (or-fail [result : (U Complex False)])
  (cond [(complex? result) result]
        [(false? result) (error "Received false when number was expected")]))

(define (the-tests) 
  (begin
    (test-case "38006F45291200, an operator packet with 2 literal subpackets reads length when lenght-ID is 0"
               (check-equal? (string->packet "38006F45291200")
                             (make-OperatorPacket 1 6 0 27 (list (make-LiteralPacket 6 4 0 0)
                                                                 (make-LiteralPacket 2 4 0 0)))))

    (test-case "get index of last bit in literal b#10111_11110_00101_000 -> 14 using 0-based position"
               (check-equal? (get-last-bit-index "101111111000101000" 0)
                             14))

    (test-case "D2FE28 -> to packet"
               (check-equal? (string->packet "D2FE28")
                             (make-LiteralPacket 6 4 0 20)))

    (test-case "or-fail throws away False in union type of string->number - i.e. (U Complex False)"
               (define result : Number (or-fail (string->number (binary-ref-most-sig "110101" 0 3) 2)))
               (check-equal? result
                             6))

    (test-case "110101 binary-ref-most-sig 0 3 -> 110 , the first 3 bits"
               (check-equal? (binary-ref-most-sig "110101" 0 3)
                             "110"))

    (test-case "hex to binary : D2FE28"
               (check-equal? (hex-string->binary-string "D2FE28")
                             "110100101111111000101000"))

    (test-case "hex to binary: 0123456789ABCDEF -> <binary-representation>"
               (define expected
                 (string-append "0000"
                                "0001"
                                "0010"
                                "0011"
                                "0100"
                                "0101"
                                "0110"
                                "0111"
                                "1000"
                                "1001"
                                "1010"
                                "1011"
                                "1100"
                                "1101"
                                "1110"
                                "1111"))

               (check-equal? (hex-string->binary-string "0123456789ABCDEF")
                             expected))

    ;; closes test-suite
    )) 

(the-tests)
