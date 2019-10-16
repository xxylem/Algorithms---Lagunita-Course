package ListsAndArrays.Sort;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;

import static ListsAndArrays.Sort.MergeSort.mergeSort;
import static org.junit.jupiter.api.Assertions.assertEquals;

class MergeSortTest {

    private LinkedList<Integer> xs, expected;

    @BeforeEach
    void setup() {
        xs = new LinkedList<>();
        expected = new LinkedList<>();
    }

    @Test
    void emptyListIsSorted() {
        assertEquals(expected, mergeSort(xs));
    }

    @Test
    void oneElementList() {
        xs.add(7);
        expected.add(7);
        assertEquals(expected, mergeSort(xs));
    }

    @Test
    void twoElementsInOrder() {
        xs.add(2);
        xs.add(3);
        expected.add(2);
        expected.add(3);
        assertEquals(expected, mergeSort(xs));
    }

    @Test
    void twoElementsOutOfOrder() {
        xs.add(3);
        xs.add(2);
        expected.add(2);
        expected.add(3);
        assertEquals(expected, mergeSort(xs));
    }

    @Test
    void longerList() {
        xs.add(7);
        xs.add(2);
        xs.add(3);
        xs.add(1);
        xs.add(9);
        xs.add(5);
        xs.add(4);
        expected.add(1);
        expected.add(2);
        expected.add(3);
        expected.add(4);
        expected.add(5);
        expected.add(7);
        expected.add(9);
        assertEquals(expected, mergeSort(xs));
    }


}
