import unittest
import random

from Lists.Select.rselect.r_select import r_select


class RSelectTest(unittest.TestCase):

    def test_empty_array_throws_value_error(self):
        with self.assertRaises(ValueError):
            r_select([], 0)

    def test_index_out_of_array_bounds(self):
        with self.assertRaises(IndexError):
            r_select([4, 6, 2, -2, 9], 5)
        with self.assertRaises(IndexError):
            r_select([6, 7, 8, 2, 3], -6)

    def test_returns_single_element_in_one_element_array(self):
        self.assertEqual(r_select([0], 0), 0)
        self.assertEqual(r_select([3], 0), 3)
        self.assertEqual(r_select([-9], 0), -9)

    def test_ordered_two_element_array(self):
        self.assertEqual(r_select([0, 1], 0), 0)
        self.assertEqual(r_select([0, 1], 1), 1)

    def test_random_long_lists(self):

        for i in range(1, 1001):
            rand_list = [random.randint(0, i) for j in range(i)]
            rand_index = random.randint(0, i - 1)
            rand_list_copy = rand_list.copy()
            r_select_res = r_select(rand_list, rand_index)
            rand_list_copy.sort()
            self.assertEqual(r_select_res, rand_list_copy[rand_index])


if __name__ == '__main__':
    unittest.main()
