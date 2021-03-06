def swap_two_elements_by_indices(integers, i, j):
    """ Input: list of n distinct integers, and two indices i and j,
                s.t. i and j are in the bounds of integers.
        Side-effect: swaps element i and element j in integers.
        Output: nothing. """
    temp = integers[j]
    integers[j] = integers[i]
    integers[i] = temp


def partition_subsection_of_list_using_pivot_index(integers, left, right, pivot_index):
    """ Input: list of n distinct integers, left and right
                    endpoints left, right in {0, 1, 2, . . . , n-1} with left <= right
                    and the index of the pivot
            Postcondition: elements of the sublist [integers[left], integers[left + 1],
                                ..., integers[right]] are partitioned around integers[pivot_index].
            Output: final position of pivot element."""
    swap_two_elements_by_indices(integers, left, pivot_index)
    return partition_subsection_of_list_with_pivot_at_left(integers, left, right)


def partition_subsection_of_list_with_pivot_at_left(integers, left, right):
    """ Input: list of n distinct integers, left and right
                endpoints left, right in {0, 1, 2, . . . , n-1} with left <= right.
        Postcondition: elements of the sublist [integers[left], integers[left + 1],
                            ..., integers[right]] are partitioned around integers[left].
        Output: final position of pivot element."""

    # The pivot is given to be the leftmost entry.
    pivot = integers[left]

    # Work through the subarray, starting after the pivot.
    # There are five key zones (which may or may not be empty) in the subarray:
    # integers[left]:                      The pivot (does not move in this loop).
    # integers[left + 1] -> integers[i - 1]:  Entries that are smaller than the pivot.
    # integers[i] -> integers[j - 1]:         Entries that are larger than the pivot.
    # integers[j]:                         The current entry being considered. If it is larger than the pivot,
    #                                       it stays here. If it is smaller, it swaps with integers[i]
    #                                       (which currently contains something larger than the pivot.
    # integers[j + 1] -> integers[right]: Entries that have not been processed yet.
    i = left + 1
    for j in range(left + 1, right + 1):
        if integers[j] < pivot:
            swap_two_elements_by_indices(integers, i, j)
            i += 1

    # Finally, swap the pivot with the last element (in terms of i) that is smaller than the pivot in the subarray.
    swap_two_elements_by_indices(integers, left, i - 1)

    # Return the pivot's new i in the array.
    return i - 1
