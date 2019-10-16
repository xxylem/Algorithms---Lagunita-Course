package ListsAndArrays.CountInversions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class SortAndCountInv {

    public static void main(String[] args) {

        List<Integer> ints = null;
        try {
            ints = Files.lines(Paths.get("Java/src/Chapter3/IntegerArray.txt"))
                    .map(Integer::parseInt)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            e.printStackTrace();
        }

        int[] intsArr = ints.stream().mapToInt(i -> i).toArray();
        InvList intsIL = new InvList(intsArr);
        InvList intsILcounted = intsIL.sortCount();
        System.out.println(intsILcounted.getInversions());


//        int[] A_arr = {6, 5, 4, 3, 2, 1};
//
//        InvList A = new InvList(A_arr);
//        InvList B = A.sortCount();
//
//        System.out.println(B);

    }

    private static class InvList {

        private long inversions;
        private int[] A;

        public InvList (int[] A){
            // Inversions are only computed when sortCount() is called.
            inversions = 0;
            this.A = A;
        }

        // Getters
        long getInversions() { return inversions; }
        int[] getArray() { return A; }
        public int getLength() { return A.length; }

        // Setter (used by mergeCountSplitInv()
        void setInversions(long invs) { inversions = invs;}

        // Returns a new InvList consisting of:
        //     A sorted version of A (the int[] array)
        //     The number of inversions found while sorting.
        private InvList sortCount() {

            int len = A.length;

            // Arrays of length 0 or 1 are already sorted and have no inversions.
            if (len < 2) {
                return this;
            }

            // Make copies of the left and right sides of the InvList array.
            int[] lefts;
            int[] rights;
            lefts = Arrays.copyOfRange(A, 0, len / 2);
            rights = Arrays.copyOfRange(A, len / 2, len);

            // Make new InvLists from the halves
            InvList C = new InvList(lefts);
            InvList D = new InvList(rights);

            // Sort the half InvLists and count the number of inversions in them.
            C = C.sortCount();
            D = D.sortCount();
            long leftInv = C.getInversions();
            long rightInv = D.getInversions();

            // ListsAndArrays.Sort.MergeSort the lists and count the split inversions.
            InvList B = mergeCountSplitInv(C, D);
            long splitInv = B.getInversions();
            B.setInversions(leftInv + rightInv + splitInv);

            return B;

        }

        private InvList mergeCountSplitInv(InvList C, InvList D) {

            int C_len = C.getLength();
            int D_len = D.getLength();

            // Compute length of new merged array.
            int n = C_len + D_len;

            int[] C_arr = C.getArray();
            int[] D_arr = D.getArray();

            // Initialise new array
            int[] B_arr = new int[n];

            // i accesses C_arr
            // j accesses D_arr
            // k accesses B_arr
            // splitInv tallies the number of inversions split across C and D
            int i = 0, j = 0, k = 0, splitInv = 0;

            // Check in all array bounds
            while (k < n && i < C_len && j < D_len) {

                // The first value of the left array is less than that of the second array,
                // so no inversion.
                if (C_arr[i] < D_arr[j]) {
                    B_arr[k] = C_arr[i];
                    i++;

                    // The first value of left array is at least that of the second array,
                    // so the val in the second array is an inversion of every element left
                    // in the left array.
                } else {
                    B_arr[k] = D_arr[j];
                    j++;
                    splitInv += (n / 2) - i;
                }
                k++;
            }

            // Some items remain in the left array - copy them to the new array.
            while (k < n && i < C_len) {
                B_arr[k] = C_arr[i];
                i++;
                k++;
            }

            // Some items remain in the right array - copy them to the new array.
            // They are not inversions because there is nothing in the left array.
            while (k < n && j < D_len) {
                B_arr[k] = D_arr[j];
                j++;
                k++;
            }

            InvList B = new InvList(B_arr);
            B.setInversions(splitInv);

            return B;
        }

        public String toString() {
            return "Array:\t\t" + Arrays.toString(A) + "\nInversions:\t" + inversions;
        }
    }
}


