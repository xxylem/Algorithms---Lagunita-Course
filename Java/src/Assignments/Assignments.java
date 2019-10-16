package Assignments;

import ListsAndArrays.CountInversions.SortAndCountInv;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public class Assignments {


    public static void main(String[] args) {

        assignment_one();

    }

    private static void assignment_one() {
        List<Integer> ints = null;
        try {
            ints = Files.lines(Paths.get("Java/src/Assignments/IntegerArray.txt"))
                    .map(Integer::parseInt)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            e.printStackTrace();
        }

        int[] intsArr = ints.stream().mapToInt(i -> i).toArray();
        SortAndCountInv.InvList intsIL = new SortAndCountInv.InvList(intsArr);
        SortAndCountInv.InvList intsILcounted = intsIL.sortCount();
        System.out.println(intsILcounted.getInversions());
    }
}
