package Matrices.Multiplication;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static Matrices.Multiplication.RecursiveMatrixMult.recMatMult;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class RecursiveMatrixMultTest {

        private int[][] X;
        private int[][] Y;

        @BeforeEach
        void setup() {

        X = new int[][] { {1, 2, 3, 4}
                        , {4, 5, 6, 2}
                        , {7, 8, 9, 7}
                        , {2, 1, 1, 0}};

        Y = new int[][] { {2, 3, 4, 3}
                        , {2, 1, 0, 9}
                        , {8, 4, 2, 3}
                        , {2, 4, 1, 6}};

        }

        @Test
        void checkMult() {

            int[][] expected = {{38, 33, 14, 54}
                    , {70, 49, 30, 87}
                    , {116, 93, 53, 162}
                    , {14, 11, 10, 18}};

            int[][] z = recMatMult(X, Y);
            assertArrayEquals(expected, z);
        }
}