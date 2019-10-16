import unittest
import random

from Lists.Select.dselect.d_select import d_select


class RSelectTest(unittest.TestCase):

    def test_empty_array_throws_value_error(self):
        with self.assertRaises(ValueError):
            d_select([], 0)

    def test_index_out_of_array_bounds(self):
        with self.assertRaises(IndexError):
            d_select([4, 6, 2, -2, 9], 5)
        with self.assertRaises(IndexError):
            d_select([6, 7, 8, 2, 3], -6)

    def test_returns_single_element_in_one_element_array(self):
        self.assertEqual(d_select([0], 0), 0)
        self.assertEqual(d_select([3], 0), 3)
        self.assertEqual(d_select([-9], 0), -9)

    def test_ordered_two_element_array(self):
        self.assertEqual(d_select([0, 1], 0), 0)
        self.assertEqual(d_select([0, 1], 1), 1)

    def test_three_element_array(self):
        self.assertEqual(d_select([1, 2, 3], 0), 1)
        self.assertEqual(d_select([1, 2, 3], 1), 2)
        self.assertEqual(d_select([1, 2, 3], 2), 3)

    def test_four_element_array(self):
        self.assertEqual(d_select([5, 6, 12, 7], 0), 5)
        self.assertEqual(d_select([5, 6, 12, 7], 1), 6)
        self.assertEqual(d_select([5, 6, 12, 7], 2), 7)
        self.assertEqual(d_select([5, 6, 12, 7], 3), 12)

    def test_random_long_lists(self):

        int_list = [x for x in range(-1000, 1000)]

        for i in range(1, 1001):

            rand_list = random.sample(int_list, i)
            rand_index = random.randint(0, i - 1)
            rand_list_copy = rand_list.copy()
            d_select_res = d_select(rand_list, rand_index)
            rand_list_copy.sort()
            self.assertEqual(d_select_res, rand_list_copy[rand_index])


if __name__ == '__main__':
    unittest.main()
