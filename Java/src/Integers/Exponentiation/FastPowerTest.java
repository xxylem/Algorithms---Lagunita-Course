package Integers.Exponentiation;

import org.junit.jupiter.api.Test;

import static Integers.Exponentiation.FastPower.fastPower;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FastPowerTest {

    private static final double DELTA = 1e-10;

    @Test
    void testOnRangeOfPositiveIntegers() {

        // Only works for small powers due to size limitations of Integer.
        for (int a = 1; a < 20; a++) {
            for (int b = 1; b < 8; b++) {
                assertEquals(fastPower(a, b), Math.pow(a, b), DELTA);
            }
        }
    }
}
