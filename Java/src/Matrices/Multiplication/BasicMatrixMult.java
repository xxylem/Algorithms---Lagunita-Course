package Matrices.Multiplication;

public class BasicMatrixMult {

    //    Input: n x n integer matrices X and Y.
    //    Output: Z = X · Y.
    static int[][] basicMatrixMult(int[][] X, int[][] Y) {

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
