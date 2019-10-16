package Integers.Multiplication;

public class RecIntMult {


    public static void main(String[] args) {

        RecIntMultVal x = new RecIntMultVal(4003, 4);
        RecIntMultVal y = new RecIntMultVal(7028, 4);

        RecIntMultVal prod = x.multiply(y);

        System.out.println("4003 * 7028 (Integers.Multiplication.RecIntMult)= ".concat(String.valueOf(prod.getVal())));

        System.out.println("Normal mult: ".concat(String.valueOf(4003 * 7028)));

    }

    private static class RecIntMultVal {

        private final int val;
        private int num_digits;
        private RecIntMultVal half1;
        private RecIntMultVal half2;

        RecIntMultVal(int val, int num_digits) {
            this.val = val;
            this.num_digits = num_digits;

            if (num_digits > 1) {
                this.split_in_two_halves();
            }
        }

        // Getters
        int getVal() { return this.val; }
        private int getNum_digits() { return this.num_digits;}
        private RecIntMultVal getHalf1() { return this.half1; }
        private RecIntMultVal getHalf2() { return this.half2; }

        // Given two positive, n-digit numbers x and y, returns x times y.
        //    ASSUMPTION: n is a power of 2.
        RecIntMultVal multiply(RecIntMultVal y) {

            // Check for numbers with different numbers of digits
            if (num_digits != y.getNum_digits()) {
                // Error TODO
            }

            // Base case: normal mult of one digit numbers
            else if (this.getNum_digits() == 1) {

                int new_val = val * y.getVal();

                return new RecIntMultVal(new_val, num_digits * 2);
            }

            // Compute partial products on the halves
            RecIntMultVal x1y1 = this.getHalf1().multiply(y.getHalf1());
            RecIntMultVal x1y2 = this.getHalf1().multiply(y.getHalf2());
            RecIntMultVal x2y1 = this.getHalf2().multiply(y.getHalf1());
            RecIntMultVal x2y2 = this.getHalf2().multiply(y.getHalf2());

            int summand1 = (int) (Math.pow(10, num_digits) * x1y1.getVal());
            int summand2 = (int) (Math.pow(10, (num_digits / 2)) * (x1y2.getVal() + x2y1.getVal()));
            int summand3 = x2y2.getVal();
            int total = summand1 + summand2 + summand3;

            return new RecIntMultVal(total, num_digits * 2);

        }

//    // Privately called by constructor to establish number of digits
//    private void setNum_digits() {
//        num_digits = String.valueOf(val).length();
//    }

        private void split_in_two_halves() {

            int b = (int) (val % Math.pow(10, num_digits / 2));
            int a = (int) (val / Math.pow(10, num_digits / 2));

            half1 = new RecIntMultVal(a, num_digits / 2);
            half2 = new RecIntMultVal(b, num_digits / 2);
        }


    }
}
