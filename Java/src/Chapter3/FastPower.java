package Chapter3;

public class FastPower {

    public static void main(String[] args) {
        System.out.println(fastPower(2, 4));
        System.out.println(fastPower(10, 5));
        System.out.println(fastPower(1, 3));
        System.out.println(fastPower(3, 3));
    }


    /** FastPower
        Input: positive integers a and b.
        Output: a^b. */
    private static int fastPower(int a, int b) {
        if (b == 1) {
            return a;
        }
        else {
            int c = a * a;
            int ans = fastPower(c, Math.floorDiv(b, 2));

            if (Math.floorMod(b, 2) == 1) {
                return a * ans;
            }
            else {
                return ans;
            }
        }
    }
}
