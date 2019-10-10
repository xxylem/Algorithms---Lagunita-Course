import random


def swap(arr_i, i, j):
    """ Input: array A of n distinct integers, and two indices i and j,
                s.t. i and j are in the bounds of A.
        Side-effect: swaps element i and element j in A.
        Output: nothing. """
    temp = arr_i[j]
    arr_i[j] = arr_i[i]
    arr_i[i] = temp


def partition(arr_i, left, right):
    """ Input: array A of n distinct integers, left and right
                endpoints left, right in {0, 1, 2, . . . , n-1} with left <= right.
        Postcondition: elements of the subarray
                        A[left],A[left + 1], . . . ,A[right] are partitioned around A[left].
        Output: final position of pivot element."""

    # The pivot is given to be the leftmost entry.
    pivot = arr_i[left]

    # Work through the subarray, starting after the pivot.
    # There are five key zones (which may or may not be empty) in the subarray:
    # arr_i[left]:                      The pivot (does not move in this loop).
    # arr_i[left + 1] -> arr_i[i - 1]:  Entries that are smaller than the pivot.
    # arr_i[i] -> arr_i[j - 1]:         Entries that are larger than the pivot.
    # arr_i[j]:                         The current entry being considered. If it is larger than the pivot,
    #                                       it stays here. If it is smaller, it swaps with arr_i[i]
    #                                       (which currently contains something larger than the pivot.
    # arr_i[j + 1] -> arr_i[right]: Entries that have not been processed yet.
    i = left + 1
    for j in range(left + 1, right + 1):
        if arr_i[j] < pivot:
            swap(arr_i, i, j)
            i += 1

    # Finally, swap the pivot with the last element (in terms of index) that is smaller than the pivot in the subarray.
    swap(arr_i, left, i - 1)

    # Return the pivot's new index in the array.
    return i - 1


def choose_pivot(left, right):
    """ Input: left and right endpoints left, right in {0, 1, 2, . . . , n-1}.
        Output: an index i in {left, left + 1, . . . , right}.
        Note: Pivot is chosen uniformly at random. """

    return random.randrange(left, right + 1)


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

