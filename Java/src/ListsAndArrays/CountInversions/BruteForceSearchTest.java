package ListsAndArrays.CountInversions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static ListsAndArrays.CountInversions.BruteForceSearch.bruteForceSearch;
import static org.junit.jupiter.api.Assertions.assertEquals;

class BruteForceSearchTest {

    private int[] empty, oneElem, twoElemNoInv, twoElemOneInv, inOrder, alternatingInversions,
                    reverseOrder, negativeInOrder, negativeOneInv;

    @BeforeEach
    void setup() {
        empty = new int[] {};
        oneElem = new int[] {7432};
        twoElemNoInv = new int[] {5, 6};
        twoElemOneInv = new int[] {6, 5};
        inOrder = new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8};
        alternatingInversions = new int[] {1, 0, 3, 2, 5, 4, 7, 6, 8};
        reverseOrder = new int[] {8, 7, 6, 5, 4, 3, 2, 1, 0};
        negativeInOrder = new int[] {-10, -9, -8, -7, -6, -5, -5, -4, -3};
        negativeOneInv = new int[] {-10, -9, -7, -8, -6, -5, -5, -4, -3};
    }

    @Test
    void emptyArrayHasNoInversions() {
        assertEquals(0, bruteForceSearch(empty));
    }

    @Test
    void oneElementArrayHasNoInversions() {
        assertEquals(0, bruteForceSearch(oneElem));
    }

    @Test
    void twoElementArrays() {
        assertEquals(0, bruteForceSearch(twoElemNoInv));
        assertEquals(1, bruteForceSearch(twoElemOneInv));
    }

    @Test
    void longerArrayInOrderHasNoInversions() {
        assertEquals(0, bruteForceSearch(inOrder));
    }

    @Test
    void arrayWithAlternatingInversions() {
        assertEquals(4, bruteForceSearch(alternatingInversions));
    }

    @Test
    void reversedListHasMaxInversions() {
        assertEquals(36, bruteForceSearch(reverseOrder));
    }

    @Test
    void negativeElements() {
        assertEquals(0, bruteForceSearch(negativeInOrder));
        assertEquals(1, bruteForceSearch(negativeOneInv));
    }
}
