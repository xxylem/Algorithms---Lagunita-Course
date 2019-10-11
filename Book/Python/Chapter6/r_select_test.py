import unittest

from Chapter6.r_select import r_select


class RSelectTest(unittest.TestCase):

    def test_empty_array_throws_value_error(self):
        self.assertRaises(ValueError, r_select, [])

    def test_returns_single_element_in_one_element_array(self):
        self.assertEqual(r_select([0]), 0)
        self.assertEqual(r_select([3]), 3)
        self.assertEqual(r_select([-9]), -9)



if __name__ == '__main__':
    unittest.main()
