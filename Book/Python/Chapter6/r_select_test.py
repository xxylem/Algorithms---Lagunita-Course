import unittest

from Chapter6.r_select import r_select


class RSelectTest(unittest.TestCase):

    def test_empty_array_throws_value_error(self):
        self.assertRaises(ValueError, r_select, [])



if __name__ == '__main__':
    unittest.main()
