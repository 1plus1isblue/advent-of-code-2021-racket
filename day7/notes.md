# Part1

Idea naive: Get range of crab locations. This is min and max. Obviously keep target within range. Crabs can move over eachother and must all end up at the same location. For each square, target,  between [min, max] consider each crab, crab_i. cost for that crab is abs(target - crab_i). Sum those up for a given target. That is the total fuel cost. Do this for all targets. Choose minimum target. This is day 1 answer.

Runtime analysis:
* For each crab, n crabs in total, and for each target, m targets in total. Will look O(n*m) times in to calculate each value
* Final pass to find min is O(n) in worst case since if new min each cell see.

Improvement: keep track of number of crabs at a given cell and use multiplication to calculate cost for that cell relative to target rather than reaccessing that memory. For example, in the test case given there are 2 crabs at location 1. The math to calculate tthat for target == 2 is the following: cost = 2 * abs(2-1). In worst case there is O(n) memory usage for this count as there is 1 crab in each cell.

Runtime analysis of improvement is still O(n*m) in worst case where there is 1 crab at each location.

Improvement idea: can I keep track of some target number and apply that with a shifting index?

Improvement idea: ternary search

# TODOs
* naive
