public class RecIntMult {


    public static void main(String[] args) {

        RecIntMultVal x = new RecIntMultVal(4003, 4);
        RecIntMultVal y = new RecIntMultVal(7028, 4);

        RecIntMultVal prod = x.multiply(y);

        System.out.println("4003 * 7028 (RecIntMult)= ".concat(String.valueOf(prod.getVal())));

        System.out.println("Normal mult: ".concat(String.valueOf(4003 * 7028)));

    }

}
