# Ideas
Can model visiting small caves only once by flipping direction of edges or pruning edges after leaving a small cave

# Algorithm outline
DFS with keeping track of path. For any given step need to keep track of what cells cannot be visited given what's already been visited. Iterating through and finding all paths. Feels like a good BFS/DFS but need to try and add branches for consideration at a given decision point.

Take the example above

```
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end
```

Example run through
Start at 'start'. Start as two options: A, b. Current path is (start). Small-nodes-visited set is (). Answer to function is sum of all paths rooted at A and b. Recurse on 

Base-case:
Reached end. That's 1 path. So return 1.
No options remaining and reached end. 0 paths found

Recurse-case:
Result is sum of recursive cases. Recurses with small-nodes-visited to indicate if a node has already been seen. This means that when entering the function we should be considering a new node.

Since there are multiple options to consider at each node we need a way to capture a loop-like traversal. Could do that with mutual recursion where there's sum-single that checks a single, and sum-multiple that iterates through all the adjacent nodes and handles. For right now will just use a for-loop and can come back later if I want. The goal is to get working stuff right now.

```

(define (sum-paths current small-visited ...)
  (cond [(null? current] 0] ;; means that we're reached the end of 
        [(string=? "end" current) 1]
        [else


        ]

  ))

  ```

# Bug part2: test-input-2.txt gets 151 paths when we expect 103
approach to solve: print out all paths found and run through verifier.

Solver found 48 bad paths. 151 - 103 = 48. Looks like the bad paths have more  than 1 small node  with 2. Need to add that rule.

```
"found 48 bad paths"
'("start" "dc" "kj" "dc" "kj" "HN" "end")
'("start" "dc" "kj" "HN" "dc" "kj" "HN" "end")
'("start" "dc" "kj" "HN" "kj" "dc" "end")
'("start" "dc" "kj" "HN" "kj" "dc" "HN" "end")
'("start" "dc" "kj" "sa" "kj" "dc" "end")
'("start" "dc" "kj" "sa" "kj" "dc" "HN" "end")
'("start" "dc" "HN" "kj" "dc" "kj" "HN" "end")
'("start" "dc" "HN" "kj" "HN" "dc" "kj" "HN" "end")
'("start" "dc" "HN" "kj" "HN" "kj" "dc" "end")
'("start" "dc" "HN" "kj" "HN" "kj" "dc" "HN" "end")
'("start" "dc" "HN" "kj" "sa" "kj" "dc" "end")
'("start" "dc" "HN" "kj" "sa" "kj" "dc" "HN" "end")
'("start" "kj" "dc" "LN" "dc" "kj" "HN" "end")
'("start" "kj" "dc" "kj" "dc" "end")
'("start" "kj" "dc" "kj" "dc" "HN" "end")
'("start" "kj" "dc" "HN" "dc" "kj" "HN" "end")
'("start" "kj" "dc" "HN" "kj" "dc" "end")
'("start" "kj" "dc" "HN" "kj" "dc" "HN" "end")
'("start" "kj" "HN" "dc" "LN" "dc" "kj" "HN" "end")
'("start" "kj" "HN" "dc" "kj" "dc" "end")
'("start" "kj" "HN" "dc" "kj" "dc" "HN" "end")
'("start" "kj" "HN" "dc" "HN" "dc" "kj" "HN" "end")
'("start" "kj" "HN" "dc" "HN" "kj" "dc" "end")
'("start" "kj" "HN" "dc" "HN" "kj" "dc" "HN" "end")
'("start" "HN" "dc" "kj" "dc" "kj" "HN" "end")
'("start" "HN" "dc" "kj" "HN" "dc" "kj" "HN" "end")
'("start" "HN" "dc" "kj" "HN" "kj" "dc" "end")
'("start" "HN" "dc" "kj" "HN" "kj" "dc" "HN" "end")
'("start" "HN" "dc" "kj" "sa" "kj" "dc" "end")
'("start" "HN" "dc" "kj" "sa" "kj" "dc" "HN" "end")
'("start" "HN" "dc" "HN" "kj" "dc" "kj" "HN" "end")
'("start" "HN" "dc" "HN" "kj" "HN" "dc" "kj" "HN" "end")
'("start" "HN" "dc" "HN" "kj" "HN" "kj" "dc" "end")
'("start" "HN" "dc" "HN" "kj" "HN" "kj" "dc" "HN" "end")
'("start" "HN" "dc" "HN" "kj" "sa" "kj" "dc" "end")
'("start" "HN" "dc" "HN" "kj" "sa" "kj" "dc" "HN" "end")
'("start" "HN" "kj" "dc" "LN" "dc" "kj" "HN" "end")
'("start" "HN" "kj" "dc" "kj" "dc" "end")
'("start" "HN" "kj" "dc" "kj" "dc" "HN" "end")
'("start" "HN" "kj" "dc" "HN" "dc" "kj" "HN" "end")
'("start" "HN" "kj" "dc" "HN" "kj" "dc" "end")
'("start" "HN" "kj" "dc" "HN" "kj" "dc" "HN" "end")
'("start" "HN" "kj" "HN" "dc" "LN" "dc" "kj" "HN" "end")
'("start" "HN" "kj" "HN" "dc" "kj" "dc" "end")
'("start" "HN" "kj" "HN" "dc" "kj" "dc" "HN" "end")
'("start" "HN" "kj" "HN" "dc" "HN" "dc" "kj" "HN" "end")
'("start" "HN" "kj" "HN" "dc" "HN" "kj" "dc" "end")
'("start" "HN" "kj" "HN" "dc" "HN" "kj" "dc" "HN" "end")
```

Problem appears in the example

```
"debug on for (start dc kj dc)"
"#hash((HN . 0) (LN . 0) (dc . 1) (end . 0) (kj . 1) (sa . 0) (start . 1))"
"opt 3 for kj with counts #hash((HN . 0) (LN . 0) (dc . 1) (end . 0) (kj . 1) (sa . 0) (start . 1))"
"valid adjacent for path (start dc kj dc) are #<set: end LN kj HN>"
"found path (start dc kj dc end)"
"found path (start dc kj dc kj HN end)"
```

```
"found path (start dc kj dc kj HN end)"
```

visits kj twice. This is a problem. From the debug println's added above we see that the visit count for dc is off-by-one - i.e. Believes dc count is only equal to 1.

The immediate fix is to calculate this everytime. I'm curious where this bug actually gets introduced though. `(dc . 1)` should actually be `(dc . 2)`.

Why isn't it incremented?

The issue was that updated-visit-count comes after valid-adjacent when valid-adjacent assumes the visit-count has already been updated. Imperative assumption killedde
me at this point.

Long-term solution: be more functional? 
