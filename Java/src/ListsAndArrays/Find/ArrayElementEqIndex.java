package ListsAndArrays.Find;

public class ArrayElementEqIndex {

    /** Given a sorted array (smallest to largest) of n distinct integers
        (positive, negative, or zero), return True if there exists an
        index i such that the element at that index is equal to the index.

        INPUT: A sorted array of n distinct integers, arr.
        OUTPUT: True if there is i s.t. arr ! i == i. False otherwise.
        ASSUMES: 0-indexing in the array. */
    static Boolean arrayElementEqIndex(int[] arr) {

        int n = arr.length;
        int windowStart = 0;
        int windowEnd = n - 1;
        int midPoint;

        switch (n) {

            case 1:
                return arr[0] == 0;

            case 2:
                return arr[0] == 0 || arr[1] == 1;

            default:

                while (windowEnd - windowStart > 1) {

                    midPoint = (windowStart + windowEnd) / 2;

                    if (arr[midPoint] == midPoint) {
                        return true;
                    }
                    else if (arr[midPoint] > midPoint) {
                        return false;
                    }
                    else {
                        windowStart = midPoint;
                    }
                }
                return arr[windowStart] == windowStart || arr[windowEnd] == windowEnd;
        }
    }
}
