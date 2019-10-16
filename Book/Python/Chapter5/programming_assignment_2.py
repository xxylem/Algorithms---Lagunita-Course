from Chapter5.quick_sort import quick_sort_with_pivot_function
from ListTools.choose_pivot import choose_first_element_as_pivot, choose_final_element_as_pivot, \
                                        choose_median_of_three_as_pivot


def main():

    f = open("QuickSort.txt", "r")
    ints = [int(x) for x in f.readlines()]
    f.close()

    # Count comparisons using first element as pivot
    comparisons_first_element = quick_sort_with_pivot_function(ints.copy(), choose_first_element_as_pivot)
    print("Using first element as pivot: ", comparisons_first_element)

    # Count comparisons using final element as pivot
    comparisons_final_element = quick_sort_with_pivot_function(ints.copy(), choose_final_element_as_pivot)
    print("Using final element as pivot: ", comparisons_final_element)

    # Choose median of three as pivot
    comparisons_median_of_three = quick_sort_with_pivot_function(ints.copy(), choose_median_of_three_as_pivot)
    print("Using median of three as pivot: ", comparisons_median_of_three)


if __name__ == '__main__':
    main()



