I want something that keeps track of the last value and considers the next one. A sequence that reads two at a time would be perfect.

I want an accumulator that keeps track of everything as it moves through.

;; Numbner Number list
(define (fn last current remainder count) ...)


Cases:
1 - No elements (fn nil nil Numbers 0) -> case 2
2 - 1st element (fn nil Number Numbers 0) -> 


199 (N/A - no previous measurement)
200 (increased)
208 (increased)
210

199 0 ;; minus 1 at end
200 199
208 200
210 208
    210

Duplicate, add element to front
Roll through at same time

While both still have elements continue

(


Ran solver
`racket solv.rkt` and got the correct answer: 1791

expected test-input.txt: 7

# Test input
```
199
200
208 -> 607
210 -> 618
200 -> 618
207 -> 617
240 -> 647
269 -> 716
260 -> 769
263 -> 792

607
618 Increase
618 
617
647 Increase
716 Increase
769 Increase
792 Increase

```
