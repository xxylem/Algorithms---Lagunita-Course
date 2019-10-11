import random
import unittest
import Chapter5.quick_sort as qs


class SwapTest(unittest.TestCase):
    def test_two_elements(self):
        a = [1, 2]
        qs.swap_two_elements_by_indices(a, 0, 1)
        b = [2, 1]
        qs.swap_two_elements_by_indices(b, 0, 1)
        c = [1, 2]
        qs.swap_two_elements_by_indices(c, 1, 0)
        d = [2, 1]
        qs.swap_two_elements_by_indices(d, 1, 0)
        self.assertEqual([2, 1], a)
        self.assertEqual([1, 2], b)
        self.assertEqual([2, 1], c)
        self.assertEqual([1, 2], d)

    def test_more_elements(self):
        a = [5, 6, 2, 6, 20, 34, 10]
        qs.swap_two_elements_by_indices(a, 2, 5)
        self.assertEqual([5, 6, 34, 6, 20, 2, 10], a)


class PartitionTest(unittest.TestCase):
    def test_two_elements(self):
        a = [1, 2]
        j = qs.partition_subsection_of_list_with_pivot_at_left(a, 0, 1)
        self.assertEqual(0, j)
        self.assertEqual([1, 2], a)

    def test_two_elem_subarray(self):
        a = [1, 2, 3, 5, 4, 6, 7, 8, 9, 10]
        j = qs.partition_subsection_of_list_with_pivot_at_left(a, 3, 6)
        self.assertEqual(4, j)
        self.assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], a)


class QuickSortTest(unittest.TestCase):
    def test_one_element(self):
        a = [1]
        qs.quick_sort(a)
        self.assertEqual(a, [1])

    def test_two_elements(self):
        a = [1, 2]
        b = [2, 1]
        qs.quick_sort(a)
        qs.quick_sort(b)
        self.assertEqual(a, [1, 2])
        self.assertEqual(b, [1, 2])

    def test_three_elements(self):
        a = [1, 2, 3]
        b = [1, 3, 2]
        c = [2, 1, 3]
        d = [2, 3, 1]
        e = [3, 1, 2]
        f = [3, 2, 1]
        qs.quick_sort(a)
        qs.quick_sort(b)
        qs.quick_sort(c)
        qs.quick_sort(d)
        qs.quick_sort(e)
        qs.quick_sort(f)
        self.assertEqual(a, [1, 2, 3])
        self.assertEqual(b, [1, 2, 3])
        self.assertEqual(c, [1, 2, 3])
        self.assertEqual(d, [1, 2, 3])
        self.assertEqual(e, [1, 2, 3])
        self.assertEqual(f, [1, 2, 3])

    def test_longer_sorts(self):
        # Generates 1000 lists of length 0-999 with random numbers 0-999
        # Sorts the list using library function
        # Sorts the list using quick_sort implementation
        # Compares results.
        for _ in range(1000):
            a = [random.randrange(1000) for _ in range(random.randrange(1000))]
            py_sorted = sorted(a)
            qs.quick_sort(a)
            self.assertEqual(py_sorted, a)


if __name__ == '__main__':
    unittest.main()
