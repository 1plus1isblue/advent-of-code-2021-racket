exponential growth :D

0 4 + 3 = 7
1 7 + 6 = 15
2 15 + 14 = 32
3 32 + 31 = 63
4 63 + 62 = 125
5 125 + 124 = 249
6 249 + 248 = 597
7 597 + 596 = 1193
8 1193 + 1192 = 2385
9 2385 + 2384 = 4769
10 4769 + 4768 = 9537

Is there a way to count letter's seen or an insight here?

Brute forcing it seems to take a lot of operations.

ideas: Each transition generates two new pairs. eg CB->H generates CH and BH and H increments by 1. Really should be incrementing H by 1 each time.

Is there a way to keep track of exponential growth? Could I cache count trees rooted at each pair?

Since deterministic rule to create I should be able to say fn(NNCB, 40) -> Number and decompose that into

merge(fn(NN, 40), fn(NC, 40), fn(CB, 40))

What about if I inverted the problem?
So CH -> B becomes B -> CH

This feels like a dynamic programming problem where I can use trees rooted at pairs to calculate subtrees.

fn(string, n) returns number of counts for letters after expanding string for n iterations. If we precompute this table we end up using O(N*M) storage and run-time in worse case and in best-case where N is the number of different nodes and M is the maximum number of iterations.

fn(NN, 40) is really

merge-counts(fn(NC, 39), fn(CN, 39))

If I follow the tree to the left

fn(NC, 39) = merge-counts(fn(NB, 38), fn(BN, 39))
Eventually we'll get fn(CH, 0) for all of them.

Can do bottom-up or top-down approach.

Finally, once we have those counts we get origina

`
merge(fn(NN, 40), fn(NC, 40), fn(CB, 40))
`

and take those counts to get the difference.

# Outline of solution
1. dynamic-program to build up counts of table using caching.
2. do merge of actual input string
3. calculate highest count
4. calculate lower count
5. return difference of highest and lowest count
