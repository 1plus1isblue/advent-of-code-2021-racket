#lang typed/racket
(require typed/rackunit
         typed/rackunit/text-ui)
(require/typed "../helpers.rkt"
               [setup-and-run (-> (-> Any) Any)])

(define-type BinaryString String)
(define-type HexString String)
(define-type Index Integer)

(define-struct Packet ([version : Number]
                       [type : Number]
                       [last-bit-pos : Integer])
                       #:transparent)

(define-struct (LiteralPacket Packet)
               ([value : Number])
               #:transparent)

(define-struct (OperatorPacket Packet)
               ([length-type : Number]
                [sub-length : Number]
                [subpackets : (Listof Packet)])
                #:transparent)

;; Intervals represented in start-end range is [inclusive,exclusive)
(define VERSION-START 0)
(define VERSION-END (+ VERSION-START 3))
(define PACKET-TYPE-START 3)
(define PACKET-TYPE-END (+ PACKET-TYPE-START 3))
(define LENGTH-ID-START 6)
(define LENGTH-ID-END (+ LENGTH-ID-START 1))
(define LENGTH-START 7)
(define LENGTH-END-ID-0 (+ LENGTH-START 15))
(define LENGTH-END-ID-1 (+ LENGTH-START 11))
(define LENGTH-ID-0-START 22)
(define LENGTH-ID-1-START 18)
(define LITERAL-VALUE-START 6)
(define TYPE-TO-OPERATOR (hash 0 '+
                               1 '*
                               2 'min
                               3 'max
                               4 'literal
                               5 '>
                               6 '<
                               7 '=))
(define OPERATOR-TO-TYPE (hash '+ 0
                               '* 1
                               'min 2
                               'max 3
                               'literal 4
                               '> 5
                               '< 6
                               '= 7))


(: type->operator (-> Number Symbol))
(define (type->operator [type : Number])
  (hash-ref TYPE-TO-OPERATOR
            type))

(: operator->type (-> Symbol Number))
(define (operator->type [op : Symbol])
  (hash-ref OPERATOR-TO-TYPE
            op))

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

(: literal-value (-> BinaryString Index Integer))
(define (literal-value [s : BinaryString] [start : Index])
  (define OFFSET-FROM-START-OF-LAST-BLOCK 4)
  (define GROUP-SIZE 5)
  (define value "")
  (do ([offset start (+ GROUP-SIZE offset)])
    ((last-group? s offset)
     (set! value (string-append value (substring s (add1 offset) (+ offset GROUP-SIZE))))
     (is-exact-integer? (or-fail (string->number value 2))))
    (set! value (string-append value (substring s (add1 offset) (+ offset GROUP-SIZE)))))
  )

(: get-last-bit-index (-> BinaryString Index Index))
(define (get-last-bit-index s value-start)
  (define OFFSET-FROM-START-OF-LAST-BLOCK 4)
  (define GROUP-SIZE 5)
  (do ([offset value-start (+ GROUP-SIZE offset)])
    ((last-group? s offset) (+ offset OFFSET-FROM-START-OF-LAST-BLOCK))))

(: hex-string->packet (-> HexString Packet))
(define (hex-string->packet hex-s) 
  (let ([binary-form : BinaryString (hex-string->binary-string hex-s)])
    (binary-string->packet binary-form 0)))

(: binary-string->packet (-> BinaryString Integer Packet))
(define (binary-string->packet [s : BinaryString] [start-index : Integer])
  (let* ([at (lambda ([b : Integer]) (+ start-index b))]
         [version-start (at VERSION-START)]
         [version-end (at VERSION-END)]
         [version : Integer (extract-integer s version-start version-end)]
         [type-start (at PACKET-TYPE-START)]
         [type-end (at PACKET-TYPE-END)]
         [type : Integer (extract-integer s type-start type-end)])
    (cond [(= type 4) (begin 
                        (define res (make-LiteralPacket version type (get-last-bit-index s (at LITERAL-VALUE-START)) (literal-value s (at LITERAL-VALUE-START))))
                             res)]
          [else (begin 
                  (define res (binary-operator-string->packet s start-index))
                  res)])))

;; Extracts an integer from a subsection of a BinaryString starting from the
;; left. For example, "1001101" 3 5, gets the middle "11" which is 3. `end`
;; is exclusive on the interval selected.
(: extract-integer (-> BinaryString Integer Integer Integer))
(define (extract-integer [s : BinaryString] [start : Integer] [end : Integer])
  (is-exact-integer? (or-fail (string->number (binary-ref-most-sig s start end) 2))))

;; Creatse a list of subpackets when length-ID == 0
(: subpackets-length-ID-0 (-> BinaryString Integer Integer (Listof Packet)))
(define (subpackets-length-ID-0 [s : BinaryString] [start : Integer] [end : Integer])
  (define subpackets : (Listof Packet) '())
  ;; bit following last bit is starting of next sub-packet
  (do ([current start (add1 (Packet-last-bit-pos (car subpackets)))])
    ((>= current end) (reverse subpackets))
    (set! subpackets (cons (binary-string->packet s current) subpackets))))

(: subpackets-length-ID-1 (-> BinaryString Integer Integer (Listof Packet)))
(define (subpackets-length-ID-1 [s : BinaryString] [start : Integer] [target-packet-count : Integer])
  (define subpackets : (Listof Packet) '())
  (do ([subpacket-start start (add1 (Packet-last-bit-pos (car subpackets)))])
    ((>= (length subpackets) target-packet-count) (reverse subpackets))
    (set! subpackets (cons (binary-string->packet s subpacket-start) subpackets))
    )
  )

;; Verifies a number is an exact integer.
(: is-exact-integer? (-> Number Integer))
(define (is-exact-integer? i)
  (cond [(exact-integer? i) i]
        [else (error (format "~a is not an exact integer" i))]))

(: binary-operator-string->packet (-> BinaryString Integer Packet))
(define (binary-operator-string->packet [s : BinaryString] [start : Integer])
  (let* ([extract (curry extract-integer s)]
         [at (lambda ([b : Integer]) (+ start b))]
         [version : Number (extract (at VERSION-START) 
                                    (at VERSION-END))]
         [type : Number (extract (at PACKET-TYPE-START)
                                 (at PACKET-TYPE-END))]
         [length-ID : Number (extract (at LENGTH-ID-START)
                                      (at LENGTH-ID-END))])
    (cond [(= length-ID 0)
           (begin 
                  (define subpacket-start : Integer (at LENGTH-ID-0-START))
                  (define subpacket-length : Integer (extract (at LENGTH-START)
                                                              (at LENGTH-END-ID-0)))
                  (define subpackets : (Listof Packet) (subpackets-length-ID-0 s 
                                                                               subpacket-start
                                                                               (+ subpacket-start subpacket-length)))
                  (make-OperatorPacket version
                                       type 
                                       (Packet-last-bit-pos (last subpackets)) 
                                       length-ID 
                                       subpacket-length
                                       subpackets))]
          [(= length-ID 1) (begin 
                  (define subpacket-start : Integer (at LENGTH-ID-1-START))
                  (define number-subpackets : Integer (extract (at LENGTH-START)
                                                              (at LENGTH-END-ID-1)))
                  (define subpackets : (Listof Packet) (subpackets-length-ID-1 s
                                                                               subpacket-start
                                                                               number-subpackets))
                  (make-OperatorPacket version
                                       type
                                       (Packet-last-bit-pos (last subpackets))
                                       length-ID
                                       number-subpackets
                                       subpackets)
                                  )]
          [else (error (format "length-ID ~a not implemented" length-ID))])))

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
  (define res (substring s start end))
  res)

(: or-fail (-> (U Complex False) Number))
(define (or-fail [result : (U Complex False)])
  (cond [(complex? result) result]
        [(false? result) (error "Received false when number was expected")]))


(: sum-versions (-> Packet Number))
(define (sum-versions packet)
  (cond [(OperatorPacket? packet) (+ (Packet-version packet)
                                     (for/sum : Number ([subpacket : Packet (in-list (OperatorPacket-subpackets packet))])
                                              (sum-versions subpacket)))]
        [else (Packet-version packet)]))

(: sum-versions-from-hex (-> HexString Number))
(define (sum-versions-from-hex hex-string)
  (sum-versions (hex-string->packet hex-string)))

(: subpackets->ast (-> (Listof Packet) (Listof Any)))
(define (subpackets->ast [packets : (Listof Packet)])
  (for/list ([sub : Packet (in-list packets)])
            (packet->ast sub)))

(: packet->ast (-> Packet Any))
(define (packet->ast [p : Packet])
  (define op (type->operator (Packet-type p)))
  (cond [(LiteralPacket? p) (LiteralPacket-value p)]
        [(and (OperatorPacket? p) (symbol=? op '+)) `(+ ,@(subpackets->ast (OperatorPacket-subpackets p)))]
        [(and (OperatorPacket? p) (symbol=? op '*)) `(* ,@(subpackets->ast (OperatorPacket-subpackets p)))]
        [(and (OperatorPacket? p) (symbol=? op 'min)) `(min ,@(subpackets->ast (OperatorPacket-subpackets p)))]
        [(and (OperatorPacket? p) (symbol=? op 'max)) `(max ,@(subpackets->ast (OperatorPacket-subpackets p)))]
        [(and (OperatorPacket? p) (symbol=? op '>)) `(if (> ,@(subpackets->ast (OperatorPacket-subpackets p))) 1 0)]
        [(and (OperatorPacket? p) (symbol=? op '<)) `(if (< ,@(subpackets->ast (OperatorPacket-subpackets p))) 1 0)]
        [(and (OperatorPacket? p) (symbol=? op '=)) `(if (= ,@(subpackets->ast (OperatorPacket-subpackets p))) 1 0)]
        [else (error (format "op ~a not implemented" op))]))

(define (the-tests) 
  (begin

    (test-case "packet->ast handles sum"
               ;; Only care about type and subpackets. Can ignore every other field
               (define in (make-OperatorPacket 0 (operator->type '+) 0 0 0 (list (make-LiteralPacket 0 0 0 1)
                                                                                 (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(+ 1 2)))

    (test-case "packet->ast literal"
               (define in (make-LiteralPacket 0 0 0 2))
               (check-equal? (packet->ast in)
                             '2))

    (test-case "packet->ast handles product"
               (define in (make-OperatorPacket 0 (operator->type '*) 0 0 0 (list (make-LiteralPacket 0 0 0 2)
                                                                                 (make-LiteralPacket 0 0 0 3))))
               (check-equal? (packet->ast in)
                             '(* 2 3)))

    (test-case "packet->ast handles minimum"
               (define in (make-OperatorPacket 0 (operator->type 'min) 0 0 0 (list (make-LiteralPacket 0 0 0 3)
                                                                                   (make-LiteralPacket 0 0 0 1)
                                                                                   (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(min 3 1 2)))

    (test-case "packet->ast handles maximum"
               (define in (make-OperatorPacket 0 (operator->type 'max) 0 0 0 (list (make-LiteralPacket 0 0 0 3)
                                                                                   (make-LiteralPacket 0 0 0 1)
                                                                                   (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(max 3 1 2)))

    (test-case "packet->ast is recursive"
               (define in (make-OperatorPacket 0 (operator->type '+) 0 0 0 (list (make-OperatorPacket 0 (operator->type '*) 0 0 0 (list (make-LiteralPacket 0 0 0 1)
                                                                                                                                        (make-LiteralPacket 0 0 0 2)))
                                                                                 (make-OperatorPacket 0 (operator->type 'min) 0 0 0 (list (make-LiteralPacket 0 0 0 3)
                                                                                                                                          (make-LiteralPacket 0 0 0 4)
                                                                                                                                          (make-LiteralPacket 0 0 0 5))))))
               (check-equal? (packet->ast in)
                             '(+ (* 1 2) (min 3 4 5))))

    (test-case "packet->ast handles greater-than"
               (define in (make-OperatorPacket 0 (operator->type '>) 0 0 0 (list (make-LiteralPacket 0 0 0 1)
                                                                                 (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(if (> 1 2) 1 0)))

    (test-case "packet->ast handles less-than"
               (define in (make-OperatorPacket 0 (operator->type '<) 0 0 0 (list (make-LiteralPacket 0 0 0 1)
                                                                                 (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(if (< 1 2) 1 0))
               )

    (test-case "packet->ast handles equal-to"
               (define in (make-OperatorPacket 0 (operator->type '=) 0 0 0 (list (make-LiteralPacket 0 0 0 1)
                                                                                 (make-LiteralPacket 0 0 0 2))))
               (check-equal? (packet->ast in)
                             '(if (= 1 2) 1 0))

               )

    (test-case "read literal b#10111_11110_00101_000 -> 2021 "
               (check-equal? (literal-value "101111111000101000" 0)
                             2021))

    (test-case "sum-version-from-hex: 8A004A801A8002F478"
               (check-equal? (sum-versions-from-hex "8A004A801A8002F478")
                             16))

    (test-case "sum-version-from-hex: 620080001611562C8802118E34"
               (check-equal? (sum-versions-from-hex "620080001611562C8802118E34")
                             12))

    (test-case "sum-versions-from-hex: C0015000016115A2E0802F182340"
               (check-equal? (sum-versions-from-hex "C0015000016115A2E0802F182340")
                             23))

    (test-case "sum-versions-from-hex: A0016C880162017C3686B18A3D4780"
               (check-equal? (sum-versions-from-hex "A0016C880162017C3686B18A3D4780")
                             31))

    (test-case "sum-version-from-hex"
               (check-equal? (sum-versions-from-hex "38006F45291200")
                             (+ 1 6 2)))

    (test-case  "sum-versions for an operator with multiple packets sums all versions"
                (define in (make-OperatorPacket 7 0 0 0 0 (list (make-LiteralPacket 2 0 0 0)
                                                                (make-LiteralPacket 4 0 0 0)
                                                                (make-LiteralPacket 1 0 0 0))))
                (check-equal? (sum-versions in)
                              (+ 7 2 4 1)))

    (test-case "sum-versions for a literal returns that version number"
               (define in (make-LiteralPacket 7 0 0 0))
               (check-equal? (sum-versions in)
                             7))
    (test-case "sum-versions for a hierarchy sums all versions"
               (define in (make-OperatorPacket 1 0 0 0 0 (list (make-OperatorPacket 2 0 0 0 0 '()))))
               (check-equal? (sum-versions in)
                             3))

    (test-case "subpackets-length-ID-1 parses 3 subpackets when starting from index 0"
               ;; 0101000000110010000010001100000110
               ;; AAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC               
               (check-equal? (subpackets-length-ID-1 "0101000000110010000010001100000110" 0 3)
                             (list (make-LiteralPacket 2 4 10 1)
                                   (make-LiteralPacket 4 4 21 2)
                                   (make-LiteralPacket 1 4 32 3))))

    (test-case "EE00D40C823060, operator packet with 3 literal subpackets and length-ID == 1"
               ;; 11101110000000001101010000001100100000100011000001100000
               ;; VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
               ;; A starts at 18
               ;; B starts at 29 
               ;; C starts at 40
               ;; top-level ends at 50
               (check-equal? (hex-string->packet "EE00D40C823060")
                             (make-OperatorPacket 7 3 50 1 3 (list (make-LiteralPacket 2 4 28 1)
                                                                   (make-LiteralPacket 4 4 39 2)
                                                                   (make-LiteralPacket 1 4 50 3))))
               )

    (test-case "38006F45291200, an operator packet with 2 literal subpackets reads length when length-ID is 0"
               ;; Packet
               ;; 00111000000000000110111101000101001010010001001000000000
               ;; VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
               ;; A starts at 22
               ;; B starts at 33
               (check-equal? (hex-string->packet "38006F45291200")
                             (make-OperatorPacket 1 6 48 0 27 (list (make-LiteralPacket 6 4 32 10)
                                                                    (make-LiteralPacket 2 4 48 20))))

               )


    (test-case "get index of last bit in literal b#10111_11110_00101_000 -> 14 using 0-based position"
               (check-equal? (get-last-bit-index "101111111000101000" 0)
                             14))

    (test-case "D2FE28 -> to packet"
               (check-equal? (hex-string->packet "D2FE28")
                             (make-LiteralPacket 6 4 20 2021)))

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

(define (solver)
  (define hex-string (car (port->lines (current-input-port))))
  (println (format "sum of versions: ~a" (sum-versions-from-hex hex-string)))
  #f
  )

(setup-and-run solver)
