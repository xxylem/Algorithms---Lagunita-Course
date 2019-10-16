package Integers.Exponentiation;

public class FastPower {

    /** FastPower
        Input: positive integers a and b.
        Output: a^b. */
    static Integer fastPower(Integer a, Integer b) {
        if (b == 1) {
            return a;
        }
        else {
            Integer c = a * a;
            Integer ans = fastPower(c, Math.floorDiv(b, 2));

            if (Math.floorMod(b, 2) == 1) {
                return a * ans;
            }
            else {
                return ans;
            }
        }
    }
}
