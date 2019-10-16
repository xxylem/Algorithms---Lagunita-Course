package Chapter3;

import java.util.Arrays;

public class BasicMatrixMult {

    public static void main(String[] args) {

        int[][] X = { { 1, 2, 3}
                    , { 4, 5, 6}
                    , { 7, 8, 9} };

        int[][] Y = { { 2, 3, 4}
                    , { 2, 1, 0}
                    , { 8, 4, 2} };

        int[][] Z = basicMatrixMult(X, Y);

        System.out.println(Arrays.deepToString(Z));

    }

    //    Input: n x n integer matrices X and Y.
    //    Output: Z = X · Y.
    private static int[][] basicMatrixMult(int[][] X, int[][] Y) {

        // Assume both matrices are n x n.
        int n = X.length;

        int[][] Z = new int[n][n];

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {

                    Z[i][j] += X[i][k] * Y[k][j];
                }
            }
        }

        return Z;
    }
}
