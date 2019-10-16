package Matrices.Multiplication;

import java.util.Arrays;

class RecursiveMatrixMult {

    //  Matrices.Multiplication.RecMatMult
    //  Input: n x n integer matrices X and Y.
    //  Output: Z = X · Y.
    //  Assumption: n is a power of 2.
    static int[][] recMatMult(int[][] X, int[][] Y) {

        // Assume both matrices are n x n.
        int n = X.length;

        int[][] Z = new int[n][n];

        // Check base case: 1x1 matrices
        if (n < 2) {
            Z[0][0] = X[0][0] * Y[0][0];
        }

        else {
            // Precompute to save divisions
            int nD2 = n / 2;

            // Split X in four equal parts
            int[][] A = getSubmatrix(X, 0, nD2, 0, nD2);
            int[][] B = getSubmatrix(X, 0, nD2, nD2, n);
            int[][] C = getSubmatrix(X, nD2, n, 0, nD2);
            int[][] D = getSubmatrix(X, nD2, n, nD2, n);

            // Split Y in four equal parts
            int[][] E = getSubmatrix(Y, 0, nD2, 0, nD2);
            int[][] F = getSubmatrix(Y, 0, nD2, nD2, n);
            int[][] G = getSubmatrix(Y, nD2, n, 0, nD2);
            int[][] H = getSubmatrix(Y, nD2, n, nD2, n);

            // Recursively compute the eight submatrix multiplications
            int[][] AE = recMatMult(A, E);
            int[][] BG = recMatMult(B, G);
            int[][] AF = recMatMult(A, F);
            int[][] BH = recMatMult(B, H);
            int[][] CE = recMatMult(C, E);
            int[][] DG = recMatMult(D, G);
            int[][] CF = recMatMult(C, F);
            int[][] DH = recMatMult(D, H);

            // Compute the additions of each part
            int[][] AEpBG = addMatrices(AE, BG);
            int[][] AFpBH = addMatrices(AF, BH);
            int[][] CEpDG = addMatrices(CE, DG);
            int[][] CFpDH = addMatrices(CF, DH);

            // Recombine the matrix from the parts.
            // This only works for n is a power of 2.
            for (int i = 0; i < nD2; i++) {
                for (int j = 0; j < nD2; j++) {
                    Z[i][j] = AEpBG[i][j];
                    Z[i][j+nD2] = AFpBH[i][j];
                    Z[i+nD2][j] = CEpDG[i][j];
                    Z[i+nD2][j+nD2] = CFpDH[i][j];
                }
            }
        }

        return Z;
    }

    // Returns the portions of the matrix X from row i1 (inclusive) to row i2 (exclusive)
    //          and from column j1 (inclusive) to column j2 (exclusive).
    private static int[][] getSubmatrix(int[][] X, int i1, int i2, int j1, int j2) {

        // Init Z to rows i1:(i2-1) from X
        int[][] Z = Arrays.copyOfRange(X, i1, i2);

        // For each row in Z, take only the column entries from j1:(j2-1)
        for (int i = 0; i < (i2 - i1); i++) {
            Z[i] = Arrays.copyOfRange(Z[i], j1, j2);
        }

        return Z;
    }

    /** Add two square n x n matrices X and Y */
    private static int[][] addMatrices(int[][] X, int[][] Y){

        int n = X.length;
        int[][] Z = new int[n][n];

        // Add entries one cell at a time.
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Z[i][j] = X[i][j] + Y[i][j];
            }
        }

        return Z;
    }

}
