def r_select(arr):

    if not arr:
        raise ValueError("Input array must be nonempty.")

    n = len(arr)

    if n == 1:
        return arr[0]
