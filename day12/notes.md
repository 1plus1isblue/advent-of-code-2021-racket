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
