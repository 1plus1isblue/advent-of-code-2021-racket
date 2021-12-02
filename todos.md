Table
[ ] - not started
[I] - in progress

# Improvements

## Rewrite day1 part2 to use window. Here's a rough sketch
Status - [ ] 

```

Example 1

199, 200, 208, 210, 200, 207, 240, 269, 260, 263, null
1st            4th

Insight is that for every value entering the tuple considered we need to compare it to the value leaving. If 
the entering value is larger than the leaving value then the new tuple will be larger than the previous tuple.
This means that the increasing count should go up by 1.

Here we use runners to keep track of the 1st and 4th element in the list.

In Example 1 the  tuple is (199, 200, 208) = 607 . The entering value 210 replaces 199. This means that the next tuple is
(200, 208, 210) = 618. And 618 > 607 so we expect the variable "increasing" to increment by 1. 

This ends when the 4th element see's null - the end of the list. At this point the algorithm is done since there are no more triplets to consider.


(define (window-handler first-element fourth-element increase-count)
  (cond
    [(null? fourth-element) increase-count]
    [(> fourth-element first-element)  (window-handler (rest first-element) (rest fourth-element) (add1 increase-count))]
    [else  (window-handler (rest first-element) (rest fourth-element) increase-count)]))
```
