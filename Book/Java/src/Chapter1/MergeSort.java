package Chapter1;

import java.util.LinkedList;

public class MergeSort {

    public static void main(String[] args) {

        LinkedList<Integer> xs = new LinkedList<>();
        xs.add(7);
        xs.add(2);
        xs.add(3);
        xs.add(1);
        xs.add(9);
        xs.add(5);
        xs.add(4);

        System.out.println(xs);
        System.out.println(mergeSort(xs));
    }

    public static LinkedList<Integer> mergeSort(LinkedList<Integer> xs) {

        int len = xs.size();

        if (len < 2) {
            return xs;
        }

        else {

            LinkedList<Integer> leftHalf = new LinkedList<>();
            LinkedList<Integer> rightHalf;

            int i = 0;
            while (i < (len / 2)) {
                leftHalf.add(xs.pop());
                i++;
            }
            rightHalf = xs;

            leftHalf = mergeSort(leftHalf);
            rightHalf = mergeSort(rightHalf);

            return merge(leftHalf, rightHalf);
        }
    }


    private static LinkedList<Integer> merge(LinkedList<Integer> xs, LinkedList<Integer> ys) {


        if (ys.isEmpty()) {
            return xs;
        }
        else if (xs.isEmpty()) {
            return ys;
        }
        else {

            Integer x = xs.getFirst();
            Integer y = ys.getFirst();

            if (x <= y) {
                xs.removeFirst();
                LinkedList<Integer> merged = merge(xs, ys);
                merged.addFirst(x);
                return merged;
            }

            else {
                ys.removeFirst();
                LinkedList<Integer> merged = merge(xs, ys);
                merged.addFirst(y);
                return merged;
            }
        }
    }
}
