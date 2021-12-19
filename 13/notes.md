tried 6e298cd10ee47e871528395a959b190b4c1d5ed8 with input.txt and got 950. Saw 950 was too high.

Tried it with test input and got 17. The correct answer.

After a single y fold the answer should be 17. 17 seems right.

Expected
#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....

expected
0,0
2,0
3,0
5,0
8,0 <- wrong
0,1
4,1
5,2
9,2
0,3
4,3
1,4
3,4
5,4
7,4
8,4
9,4

actual
doesn't matter. Doesn't explain count difference. Try a couple different y folds

diff
5,0
8,0
5,2
9,2
5,4
7,4


6,0
9,0
6,2
10,2
6,4
10,4

diff
Xs have changed. This seems wrong

After x fold? there should be 16 dots

Looks like I need to implement fold-x since part1 asks for the count after the 1st fold

Where does each element in
```
missing #<set: #(struct:posn 8 0) #(struct:posn 5 4) #(struct:posn 5 0) #(struct:posn 5 2) #(struct:posn 7 4) #(struct:posn 9 2)>
```

come from?

8,0 from below line at 
```

Fuck. Wrong place to get expected

```
#.##..#..#. 
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
```
Expected now
0,0
2,0
3,0
6,0
9,0
0,1
4,1
6,2
10,2
0,3
4,3
1,4
3,4
6,4
8,4
9,4
10,4

For test on folding via x. Expected
```
#####
#...#
#...#
#...#
#####
.....
.....

```
0,0
1,0
2,0
3,0
4,0
0,1
4,1
0,2
4,2
0,3
4,3
0,4
1,4
2,4
3,4
4,4
