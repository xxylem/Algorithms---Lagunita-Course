from ListTools.list_tools import swap_two_elements_by_indices, partition_subsection_of_list_with_pivot_at_left, choose_random_pivot


def quick_sort(integers):
    """ Input: list of n distinct integers
        Side-effect: elements of the list are sorted from smallest to largest."""

    def helper(left, right):
        # Used to avoid needing left and right params in the main function.
        # The helper function works on the subarray that is integers between indices left and right, inclusive.

        if left >= right:
            # 0 or 1 element list, no sorting to do.
            return

        # Select a pivot and move it to the front of the subarray.
        i = choose_random_pivot(left, right)
        swap_two_elements_by_indices(integers, left, i)

        # Partition the subarray based on the pivot
        j = partition_subsection_of_list_with_pivot_at_left(integers, left, right)

        # Recursively sort the subarrays either side of the pivot.
        helper(left, j - 1)
        helper(j + 1, right)

    # Initially call the helper function with the full range of the array, using inclusive indices with 0-indexing.
    helper(0, len(integers) - 1)

