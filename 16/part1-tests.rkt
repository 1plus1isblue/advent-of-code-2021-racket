#lang typed/racket
(require "../helpers.rkt"
         typed/rackunit
         typed/rackunit/text-ui)

(define-struct LiteralPacket ([version : (U Number False)] [type : (U Number False)] [value : Any]) #:transparent)
(define-type BinaryString String) ; just an alias for String
(define-type HexString String) ; just an alias for String

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


(: string->packet (-> String (Struct LiteralPacket)))
(define (string->packet s) 
  (let* ([binary-form : BinaryString (hex-string->binary-string s)]
         [version-bits : BinaryString (binary-ref-most-sig binary-form 0 3)]
         [version : Number (or-fail (string->number version-bits 2))]
         [type-bits : BinaryString (binary-ref-most-sig binary-form 3 6)]
         [type : Number (or-fail (string->number type-bits 2))])
    (make-LiteralPacket 
      version 
      type
      0)))

;; Converts a string representation of a hex to a binary representation of it.
;; Example "BD" -> "10111101"
(: hex-string->binary-string (-> HexString BinaryString))
(define (hex-string->binary-string s)
  (foldr string-append
         ""
         (map hex-to-binary
              (map string (string->list s)))))

;; Gets [start, end] binary digits from left to right of the BinaryString
(: binary-ref-most-sig (-> BinaryString Nonnegative-Integer Positive-Integer BinaryString))
(define (binary-ref-most-sig [s : BinaryString]
                        [start : Nonnegative-Integer]
                        [end : Positive-Integer])
  (substring s start end))

(: or-fail (-> (U Complex False) Number))
(define (or-fail [result : (U Complex False)])
  (cond [(complex? result) result]
        [(false? result) (error "Received false when number was expected")]))

(define tests
  (test-suite
    "tests"

    (test-case "")

    (test-case "D2FE28 -> to packet"
               (check-equal? (string->packet "D2FE28")
                             (make-LiteralPacket 6 4 0)))

    (test-case "D2 -> gets sucked up as a packet with version 6 and type 4"
               (check-equal? (string->packet "D2")
                             (make-LiteralPacket 6 4 0)))

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

(run-tests tests)
