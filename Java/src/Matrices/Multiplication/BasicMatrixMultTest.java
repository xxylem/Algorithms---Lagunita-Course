package Matrices.Multiplication;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static Matrices.Multiplication.BasicMatrixMult.basicMatrixMult;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class BasicMatrixMultTest {

    private int[][] X;
    private int[][] Y;

    @BeforeEach
    void setup() {

    X = new int[][] { { 1, 2, 3}
                    , { 4, 5, 6}
                    , { 7, 8, 9} };

    Y = new int[][] { { 2, 3, 4}
                    , { 2, 1, 0}
                    , { 8, 4, 2} };

    }

    @Test
    void checkMult() {

        int[][] expected = { { 30, 17, 10}
                           , { 66, 41, 28}
                           , {102, 65, 46} };

        int[][] z = basicMatrixMult(X, Y);
        assertArrayEquals(expected, z);
    }

}
