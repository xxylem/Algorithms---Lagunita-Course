def _number_of_digits(x):
    """ Input: n-digit number x
        Output: n """

    if x == 0:
        return 1

    num_digits = 0
    while x > 0:
        x = x // 10
        num_digits += 1

    return num_digits


def _split_int_in_two(x):
    """
    Splits the decimal representation of n-digit number x in two. E.g.: 1234 -> (12, 34)
    ASSUMES: n is even
    :param x: An n-digit number
    :return: first half: The first half of x (n/2 digits)
             second half: The second half of x (n/2 digits)
    """

    n = _number_of_digits(x)
    modn2 = 10 ** (n // 2)

    first_half = x // modn2
    second_half = x % modn2

    return first_half, second_half


def rec_int_mult(x, y):
    """ Input: two n-digit positive integers x and y.
        Output: the product x · y.
        Assumption: n is a power of 2."""

    n = _number_of_digits(x)

    if n == 1:
        return x * y

    else:
        (a, b) = _split_int_in_two(x)
        (c, d) = _split_int_in_two(y)

        ac = rec_int_mult(a, c)
        ad = rec_int_mult(a, d)
        bc = rec_int_mult(b, c)
        bd = rec_int_mult(b, d)

        return (10 ** n) * ac + (10 ** (n//2)) * (ad + bc) + bd
