import unittest
import Chapter1.rec_int_mult as rim


class MyTestCase(unittest.TestCase):
    def test_single_digit(self):
        self.assertEqual(rim.rec_int_mult(1, 2), 1 * 2)
        self.assertEqual(rim.rec_int_mult(0, 4), 0 * 4)
        self.assertEqual(rim.rec_int_mult(9, 9), 9 * 9)
        self.assertEqual(rim.rec_int_mult(3, 2), 3 * 2)
        self.assertEqual(rim.rec_int_mult(6, 4), 6 * 4)

    def test_two_digits(self):
        self.assertEqual(rim.rec_int_mult(22, 19), 22 * 19)
        self.assertEqual(rim.rec_int_mult(23, 19), 23 * 19)
        self.assertEqual(rim.rec_int_mult(99, 99), 99 * 99)
        self.assertEqual(rim.rec_int_mult(13, 65), 13 * 65)

    def test_larger_n(self):
        self.assertEqual(rim.rec_int_mult(1234, 5678), 1234 * 5678)
        self.assertEqual(rim.rec_int_mult(95736523, 90781534), 95736523 * 90781534)


if __name__ == '__main__':
    unittest.main()
