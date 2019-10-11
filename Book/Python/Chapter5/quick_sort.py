from ListTools.list_tools import swap, partition, choose_pivot


def quick_sort(arr_i):
    """ Input: array A of n distinct integers, left and right
                endpoints left, right in {0, 1, 2, . . . , n-1} (0-indexing).
        Postcondition: elements of the subarray
                        A[left],A[left + 1], . . . ,A[right] are sorted from smallest to
                        largest."""

    def helper(left, right):
        # Used to avoid needing left and right params in the main function.
        # The helper function works on the subarray that is arr_i between indices left and right, inclusive.

        if left >= right:
            # 0 or 1 element list, no sorting to do.
            return

        # Select a pivot and move it to the front of the subarray.
        i = choose_pivot(left, right)
        swap(arr_i, left, i)

        # Partition the subarray based on the pivot
        j = partition(arr_i, left, right)

        # Recursively sort the subarrays either side of the pivot.
        helper(left, j - 1)
        helper(j + 1, right)

    # Initially call the helper function with the full range of the array, using inclusive indices with 0-indexing.
    helper(0, len(arr_i) - 1)

