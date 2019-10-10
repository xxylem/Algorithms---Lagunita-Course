package Chapter5;

import java.util.Random;

public class QuickSort {

    /** Swaps the elements at indices i and j in the array arr. */
    private static void swap(int[] arr, int i, int j){
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    /** Input: array A of n distinct integers, left and right
                endpoints left, right in {0, 1, 2, . . . , n-1} with left <= right.
        Postcondition: elements of the subarray
            A[left],A[left + 1], . . . ,A[right] are partitioned around A[left].
        Output: final position of pivot element.*/
    private static int partition(int[] arr, int left, int right){

        // The pivot is given to be the leftmost entry.
        int pivot = arr[left];

        // Work through the subarray, starting after the pivot.
        // There are five key zones (which may or may not be empty) in the subarray:
        // arr_i[left]:                      The pivot (does not move in this loop).
        // arr_i[left + 1] -> arr_i[i - 1]:  Entries that are smaller than the pivot.
        // arr_i[i] -> arr_i[j - 1]:         Entries that are larger than the pivot.
        // arr_i[j]:                         The current entry being considered. If it is larger than the pivot,
        //                                       it stays here. If it is smaller, it swaps with arr_i[i]
        //                                       (which currently contains something larger than the pivot.
        // arr_i[j + 1] -> arr_i[right]: Entries that have not been processed yet.
        int i = left + 1;
        for (int j = left + 1; j <= right; j++) {
            if (arr[j] < pivot) {
                swap(arr, i, j);
                i++;
            }
        }

    // Finally, swap the pivot with the last element (in terms of index) that is smaller than the pivot in the subarray.
    swap(arr, left, i - 1);

    // Return the pivot's new index in the array.
    return i - 1;
    }

    /** Choose the index for the pivot between left and right, inclusive */
    private static int choosePivot(int left, int right) {

        Random r = new Random();
        return left + r.nextInt(1 + right - left);
    }

    /** Used to avoid needing left and right params in the main function.
        The helper function works on the subarray that is arr_i between
        indices left and right, inclusive. */
    private static void quickSortHelper(int[] arr, int left, int right) {

        if (left >= right) {
            //0 or 1 element list, no sorting to do.
            return;
        }

        // Select a pivot and move it to the front of the subarray.
        int i = choosePivot(left, right);
        swap(arr, left, i);

        // Partition the subarray based on the pivot
        int j = partition(arr, left, right);

        // Recursively sort the subarrays either side of the pivot.
        quickSortHelper(arr, left, j - 1);
        quickSortHelper(arr, j + 1, right);
    }

    /** Input: array A of n distinct integers, left and right
                endpoints left, right in {0, 1, 2, . . . , n-1} (0-indexing).
        Postcondition: elements of the subarray
            A[left], A[left + 1], . . . ,A[right] are sorted from smallest to
            largest.*/
    public static void quickSort (int[] arr) {


    // Initially call the helper function with the full range of the array
        // using inclusive indices with 0-indexing.
    quickSortHelper(arr, 0, arr.length - 1);
    }

}
