import random

import Chapter5.quick_sort


def choose_random_pivot(left, right, integers):
    """ Input: left and right endpoints left, right in {0, 1, 2, . . . , n-1}.
        Output: an i in {left, left + 1, . . . , right}.
        Note: Pivot is chosen uniformly at random. """

    return random.randrange(left, right + 1)   # (right + 1) because the upper bound in randrange is exclusive.


def choose_first_element_as_pivot(left, right, integers):
    """"left and right endpoints left, right in {0, 1, 2, . . . , n-1}.
        Output: left """
    return left


def choose_final_element_as_pivot(left, right, integers):
    """"left and right endpoints left, right in {0, 1, 2, . . . , n-1}.
            Output: right """
    return right


def choose_median_of_three_as_pivot(left, right, integers):

    mid_index = (left + right) // 2

    median_indexes = {
        integers[left]: left,
        integers[mid_index]: mid_index,
        integers[right]: right
    }

    medians = [integers[left],
               integers[(left + right) // 2],
               integers[right]]

    Chapter5.quick_sort.quick_sort(medians)

    return median_indexes[medians[1]]