def r_select(array, index):

    if not array:
        raise ValueError("Input array must be nonempty.")

    array_length = len(array)

    if array_length == 1:
        return array[0]



