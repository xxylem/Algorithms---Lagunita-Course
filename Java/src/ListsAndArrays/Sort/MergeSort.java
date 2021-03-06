package ListsAndArrays.Sort;

import java.util.LinkedList;

public class MergeSort {

    /** Returns the list xs sorted in ascending order.
     *  SIDE-EFFECTS: May destroy the input list. Use a copy if you want to preserve the original. */
    static LinkedList<Integer> mergeSort(LinkedList<Integer> xs) {

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
