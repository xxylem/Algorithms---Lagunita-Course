public class BruteForceSearch {

    public static void main(String[] args) {

        int[] xs = {0, 1, 2, 3, 4, 5, 6, 7, 8};
        int[] ys = {1, 0, 3, 2, 5, 4, 7, 6, 8};
        int[] zs = {8, 7, 6, 5, 4, 3, 2, 1, 0};

        int xsNInvs = bruteForceSearch(xs);
        int ysNInvs = bruteForceSearch(ys);
        int zsNInvs = bruteForceSearch(zs);

        System.out.println("Xs: ");
        System.out.println(xsNInvs);
        System.out.println("Ys: ");
        System.out.println(ysNInvs);
        System.out.println("Zs: ");
        System.out.println(zsNInvs);

    }

    /**
     * Brute-Force Search for Counting Inversions
     *
     * @param xs
     * Input: array A of n distinct integers.
     *
     * @return numInv
     * Output: the number of inversions of A.
     */
    public static int bruteForceSearch(int[] xs) {

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
