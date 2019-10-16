package ListsAndArrays.Find;

public class ArrayElementEqIndex {

    public static void main(String[] args) {


        // Examples of sorted arrays.
        int[] arr1 = {0};
        System.out.println(arrayElementEqIndex(arr1));
        int[] arr2 = {1};
        System.out.println(arrayElementEqIndex(arr2));
        int[] arr3 = {1, 2};
        System.out.println(arrayElementEqIndex(arr3));
        int[] arr4 = {0, 2};
        System.out.println(arrayElementEqIndex(arr4));
        int[] arr5 = {-1, 1};
        System.out.println(arrayElementEqIndex(arr5));
        int[] arr6 = {-1, 0, 1};
        System.out.println(arrayElementEqIndex(arr6));
        int[] arr7 = {-1, 0, 2};
        System.out.println(arrayElementEqIndex(arr7));
        int[] arr8 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        System.out.println(arrayElementEqIndex(arr8));
        int[] arr9 = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        System.out.println(arrayElementEqIndex(arr9));
        int[] arr10 = {-10, -9, -8, -7, -6, -5, -4, -3, -2, -1};
        System.out.println(arrayElementEqIndex(arr10));
        int[] arr11 = {-10, -9, -8, -7, -6, -5, -4, -3, -2, 9};
        System.out.println(arrayElementEqIndex(arr11));

    }

    /** Given a sorted array (smallest to largest) of n distinct integers
        (positive, negative, or zero), return True if there exists an
        index i such that the element at that index is equal to the index.

        INPUT: A sorted array of n distinct integers, arr.
        OUTPUT: True if there is i s.t. arr ! i == i. False otherwise.
        ASSUMES: 0-indexing in the array. */
    private static Boolean arrayElementEqIndex (int[] arr) {

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
