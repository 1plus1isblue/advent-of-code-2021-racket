# TODOs Part 2
* [X] Increment all cells in grid by 1 and wrap around at 10
* [X] generate 9 forms in a linear array
* [X] create hand-crafted array showing subarrays contained
* [X] map example forms into hand-crafted array showing subarrays
* [X] stitch subgrids together
  * [X] stitch columns together
  * [X] stitch rows together
* [X] write up instructions
* [X] enter 2905 into day 15 part 2 - blocked on needing internet

# Usage
1. generate `output.txt` which contains the full full grid

`
> racket part2.rkt input.txt
`

The results of part2.rkt are written to `output.txt` in the current directory after running the script

2. direct the fullsized grid to the solver from part 1 for the solution

`
> racket part1.rkt output.txt
...
cpu...
<number>
#f
`

The `<number>` is the solution

# Notes
Solution is dynamic programming

# Stitching subgrids together

Grid 0
01 ab
23 cd

ab 45
cd 67

Should become Grid 1
  0123
  ----
0|01ab
1|23cd
2|ab45
3|cd67

Indexing for grid 0

  0123
  ----
0|0011
1|0011
2|2233
3|2233

Easiest to merge rows via (vector-append ...). Something like (apply vector-append '(list-of-vectors-on-row)) for each meta-row. This will bridge columns. So now Grid 0 looks like 

Grid 2

01ab
23cd

ab45
cd67

At this point we'll need to merge the meta-rows. Maybe this could coded more easily with recursion?

Just do it with one applycation of vector-append for each set of rows. This is the easiest step.

# Problem with 2905
Submitted 2905 on advent for day 15 and got "too high". Did I remove the very first cell?

Check the example again and try a couple different variations.

1. [X] Confirm that part1.rkt works with input given on day 15 part 2
2. [X] Confirm that part2.rkt on test-input-1.txt gives same result as day 15 part 2 example input

Ideas:
* [X] Provide example where up is valid. If so time to use djikstra's and represent problem as a graph. See test-input-5.txt

* [X] Convert input to graph and pass to shortest-algorithm

# Problem with 2849
too low for me but correct answer for someone else.

Try and get the online input again and confirm my input.txt file is the same.

Also, my dijkstra's finds a lower minimal total cost when using my first input. I'm going to try restarting the webpage and downloading the input again.

When tested against the test input, test-input-1.txt everything seems fine. When tested against my original input, input-v1.txt, I get 582. Which is 10 less than the actual answer.

When tested against the expanded version, test-input-3.txt, I get 312 - 3 less than the actual answer.

[X] Next step is to print out the found path and figure out how it differs. Verify that we're not moving diagonally.

The fact that test-input-3.txt results in 312 rather than 315 when given to part1.rkt suggests that I need to see the actual path taken. 

```
1                                                 
1                                                 
2                                                 
3                                                 
7                                                 
1                                                 
1                                                 
3                                                 
1                                                 
2                                                 
2                                                 
2                                                 
324                                               
  15                                              
   4                                              
   1                                              
   1123532                                        
         1                                        
         2342                                     
            332                                   
              1                                   
              61                                  
               44                                 
                4                                 
                1                                 
                2461                              
                   4                              
                   3                              
                   4564                           
                      554                         
                        3163                      
                           2                      
                           8                      
                           125                    
                             6413                 
                                7                 
                                26                
                                 21               
                                  7               
                                  6112            
                                     5            
                                     4            
                                     1            
                                     34725        
                                         3        
                                         2        
                                         24  9131 
                                          1431  2 
                                                1 
                                                79
```

The issue is the last chain just before the end. My algorithm suggests 913121 but the answer in the expanded example is 2334. 2+3+3+4 = 12 and 9+1+3+1+2+1 = 24. 12 is obviously lower than 24 so I'm not sure
a) why part1 gives a lower total cost than the expected
b) why the path with 12 is chosen over 24. That seems MORE expensive.

```
   9131 
1431  2 
      1 
      79
```

Should actually be

```
1431
   2
   33479
```

Debug with a small test input.

```
48891319
14319528
82927419
99833479
```

So after creating smaller output the wrong path with the wrong total is still used. What does the graph look like?

[X] Get edge-weight seen for these edges. The total isn't even making sense.

Looked at edge weights where the path deviates. This is at (3,1). At (3,1) we expect the next step to be (3,2) since the value there is 2 and dijkstra's is greedy; instead, what we choose goes to (3,0) which has a value of 9. When we print out the weights for the edges we see the following

```
...
(1 (3 0) (3 1))
...
(2 (3 1) (3 2))
```

This says `(<weight> <a> <b>)`. This is completely wrong. The connections should go both ways. so `(9 (3 1) (3 0))` and `(1 (3 0) (3 1))`. So I'm going to retry the grid->graph code.

Yup. That fixed it. Lessons here:
1. don't code drunk kids. Mezcal & algorithms don't mix well
2. more toy test examples to reason about grid. I used an unweighted grid and thought "both" directions would be encapsulated. Lazy thinking on my part. tsk tsk.
