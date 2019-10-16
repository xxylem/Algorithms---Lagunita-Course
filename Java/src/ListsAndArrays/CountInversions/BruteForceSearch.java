package ListsAndArrays.CountInversions;

public class BruteForceSearch {

     /**
     * Brute-Force Search for Counting Inversions
     *
     * @param xs
     * Input: array A of n distinct integers.
     *
     * @return numInv
     * Output: the number of inversions of A.
     */
    static int bruteForceSearch(int[] xs) {

        int numInv = 0;
        int n = xs.length;

        for (int i = 0; i < n - 1; i++) {
            for (int j = i + 1; j < n; j++) {
                if (xs[i] > xs [j]) {
                    numInv++;
                }
            }
        }

        return numInv;
    }
}
