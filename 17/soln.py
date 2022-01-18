import operator
import functools
import unittest
from expected import VALID_INITIAL_VELOCITIES

class Solver(object):
    def __init__(self, min_x, max_x, min_y, max_y):
        self.min_x = min_x
        self.max_x = max_x
        self.min_y = min_y
        self.max_y = max_y

    def is_in_bounds(self, position):
        return position[0] <= self.max_x and position[1] >= self.min_y
    
    def is_in_target(self, position):
        x = position[0]
        y = position[1]
        return x <= self.max_x and x >= self.min_x and y <= self.max_y and y >= self.min_y
    
    def next_velocity(self, v):
        y_new = v[1] - 1
        x_sign = 1 if v[0] >= 0 else -1
        x_new = 0 if v[0] == 0 else x_sign * (abs(v[0]) - 1)
        return (x_new, y_new)
    
    def step(self, position, velocity):
        pos = tuple(map(operator.add, position, velocity))
        vel = self.next_velocity(velocity)
        return pos, vel
    
    def is_valid(self, position):
        return self.is_in_bounds(position) and self.is_in_target(position)
    
    def has_another_iteration(self, position):
        return self.is_in_bounds(position) and not self.is_in_target(position)
    
    def run_to_target(self, x, y):
        position = (0, 0)
        velocity = (x, y)
        max_y = -1
        while self.has_another_iteration(position):
            position, velocity = self.step(position, velocity)
            max_y = max(position[1], max_y)
        return position, max_y

    def y_iters(self):
        # maximum height is largest of absolute value of both y values
        # because since the target is below, in my input, we need to account
        # for the fact that each step y decreases by 1. This means that
        # by the time the project passes y == 0 on the way down the
        # velocity will be the negative of the initial starting y.
        return max(abs(self.min_y), abs(self.max_y))

    def check_valid_range(self):
        res = []
        for x in range(self.max_x+1):
            for y in range(-self.y_iters(), self.y_iters()+1): # TODO can I remove the +1?
                pos, max_y = self.run_to_target(x, y)
                if self.is_in_target(pos):
                    res.append(((x, y), max_y))
                    continue
        return res

    def count_valid_init_velocities(self):
        return len(self.check_valid_range())

class Tests(unittest.TestCase):
    def test_is_valid_0_0_returns_false(self):
        solver = Solver(20, 30, -10, -5)
        self.assertFalse(solver.is_valid((0,0)))

    def test_run_to_target_7_2_stops_at_28_neg_7(self):
        solver = Solver(20, 30, -10, -5)
        self.assertEqual(solver.run_to_target(7,2), ((28, -7), 3))

    def test_is_valid_7_2_returns_true(self):
        solver = Solver(20, 30, -10, -5)
        pos, max_y = solver.run_to_target(7,2)
        self.assertTrue(solver.is_valid(pos))

    def test_run_to_target_6_3_stops_at_21_neg_9(self):
        solver = Solver(20, 30, -10, -5)
        self.assertEqual(solver.run_to_target(6,3), ((21,-9), 6))

    def test_is_valid_17_neg_4_returns_false(self):
        solver = Solver(20, 30, -10, -5)
        self.assertFalse(solver.is_valid((17,-4)))

    def test_run_to_target_6_9_returns_max_y_of_45(self):
        solver = Solver(20, 30, -10, -5)
        self.assertEqual(solver.run_to_target(6, 9)[1], 45)

    def test_check_targets(self):
        solver = Solver(20, 30, -10, -5)
        results = solver.check_valid_range()
        max_ys = list(map(lambda res: res[1], results))
        self.assertEqual(functools.reduce(lambda a, b: max(a,b), max_ys), 45)

    def test_part_1_input(self):
        solver = Solver(175, 227, -134, -79)
        results = solver.check_valid_range()
        max_ys = list(map(lambda res: res[1], results))
        self.assertEqual(functools.reduce(lambda a, b: max(a, b), max_ys), 8911)

    def test_find_difference_between_valid_points_and_expected(self):
        solver = Solver(20, 30, -10, -5)
        results = solver.check_valid_range()
        points = {v for v in map(lambda res: res[0], results)}
        self.assertEqual(points, VALID_INITIAL_VELOCITIES)

    def test_is_valid_8_0_returns_true(self):
        solver = Solver(20, 30, -10, -5)
        #import pdb; pdb.set_trace()
        results = solver.check_valid_range()
        points = {v for v in map(lambda res: res[0], results)}
        self.assertIn((8,0), points)

    def test_number_of_results_test_input_is_112(self):
        solver = Solver(20, 30, -10, -5)
        results = solver.count_valid_init_velocities()
        self.assertEqual(results, 112)

    def test_part_2_input(self):
        solver = Solver(175, 227, -134, -79)
        results = solver.check_valid_range()
        valid_starting_velocities = list(map(lambda res: res[0], results))
        self.assertEqual(len(valid_starting_velocities), 4748)

if __name__ == '__main__':
    unittest.main()
