package ListsAndArrays.Find;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static ListsAndArrays.Find.ArrayElementEqIndex.arrayElementEqIndex;
import static org.junit.jupiter.api.Assertions.*;

class ArrayElementEqIndexTest {

    private int[] oneElemT, oneElemF, twoElemPosF, twoElem0T, twoElemNegT, threeElemF, threeElemT, longerF, longerT,
                    negLongerF, negLongerT;

    @BeforeEach
    void setup() {
        oneElemT = new int[] {0};
        oneElemF = new int[] {1};
        twoElemPosF = new int[] {1, 2};
        twoElem0T = new int[] {0, 2};
        twoElemNegT = new int[] {-1, 1};
        threeElemF = new int[] {-1, 0, 1};
        threeElemT = new int[] {-1, 0, 2};
        longerF = new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        longerT = new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        negLongerF = new int[] {-10, -9, -8, -7, -6, -5, -4, -3, -2, -1};
        negLongerT = new int[] {-10, -9, -8, -7, -6, -5, -4, -3, -2, 9};
    }

    @Test
    void oneElementArrays() {
        assertTrue(arrayElementEqIndex(oneElemT));
        assertFalse(arrayElementEqIndex(oneElemF));
    }

    @Test
    void twoElementArrays() {
        assertFalse(arrayElementEqIndex(twoElemPosF));
        assertTrue(arrayElementEqIndex(twoElem0T));
        assertTrue(arrayElementEqIndex(twoElemNegT));
    }

    @Test
    void threeElementArrays() {
        assertFalse(arrayElementEqIndex(threeElemF));
        assertTrue(arrayElementEqIndex(threeElemT));
    }

    @Test
    void longerArrays() {
        assertFalse(arrayElementEqIndex(longerF));
        assertTrue(arrayElementEqIndex(longerT));
    }

    @Test
    void arraysOfNegativeElements() {
        assertFalse(arrayElementEqIndex(negLongerF));
        assertTrue(arrayElementEqIndex(negLongerT));
    }

}
