package ListsAndArrays.CountInversions;

import ListsAndArrays.CountInversions.SortAndCountInv.InvList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SortAndCountInvTest {

    InvList empty, oneElem, twoElemNoInv, twoElemOneInv, longerSorted, longerReverseSorted, longerAlternatingInvs,
             negativeOneElem, negativeTwoElemNoInv, negativeTwoElemOneInv;
    InvList sortedInvList;

    @BeforeEach
    void setup() {
        empty = new InvList(new int[] {});
        oneElem = new InvList(new int[] {5});
        twoElemNoInv = new InvList(new int[] {5, 6});
        twoElemOneInv = new InvList(new int[] {6, 5});
        longerSorted = new InvList(new int[] {1, 2, 3, 4, 5, 6, 7, 8});
        longerReverseSorted = new InvList(new int[] {8, 7, 6, 5, 4, 3, 2, 1});
        longerAlternatingInvs = new InvList(new int[] {2, 1, 4, 3, 6, 5, 8, 7});
        negativeOneElem = new InvList(new int[] {-6});
        negativeTwoElemNoInv = new InvList(new int[] {-12, -11});
        negativeTwoElemOneInv = new InvList(new int[] {-11, -12});
    }

    @Test
    void emptyListHasNoInversions() {
        sortedInvList = empty.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {}, sortedInvList.getArray());
    }

    @Test
    void oneElementListHasNoInversions() {
        sortedInvList = oneElem.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {5}, sortedInvList.getArray());
    }

    @Test
    void twoElementLists() {
        sortedInvList = twoElemNoInv.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {5, 6}, sortedInvList.getArray());

        sortedInvList = twoElemOneInv.sortCount();
        assertEquals(1, sortedInvList.getInversions());
        assertArrayEquals(new int[] {5, 6}, sortedInvList.getArray());
    }

    @Test
    void sortedListsHaveNoInversions() {
        sortedInvList = longerSorted.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {1, 2, 3, 4, 5, 6, 7, 8}, sortedInvList.getArray());
    }

    @Test
    void reverseSortedListsHaveMaxInversions() {
        sortedInvList = longerReverseSorted.sortCount();
        assertEquals(28, sortedInvList.getInversions());
        assertArrayEquals(new int[] {1, 2, 3, 4, 5, 6, 7, 8}, sortedInvList.getArray());
    }

    @Test
    void alternatingInversions() {
        sortedInvList = longerAlternatingInvs.sortCount();
        assertEquals(4, sortedInvList.getInversions());
        assertArrayEquals(new int[] {1, 2, 3, 4, 5, 6, 7, 8}, sortedInvList.getArray());
    }

    @Test
    void arrraysWithNegativeElements() {
        sortedInvList = negativeOneElem.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {-6}, sortedInvList.getArray());

        sortedInvList = negativeTwoElemNoInv.sortCount();
        assertEquals(0, sortedInvList.getInversions());
        assertArrayEquals(new int[] {-12, -11}, sortedInvList.getArray());

        sortedInvList = negativeTwoElemOneInv.sortCount();
        assertEquals(1, sortedInvList.getInversions());
        assertArrayEquals(new int[] {-12, -11}, sortedInvList.getArray());
    }
}
