package ListsAndArrays.Find;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static ListsAndArrays.Find.UnimodalMax.unimodalArrayMaxElement;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class UnimodalMaxTest {

    int[] oneElem, twoelem, threeElemUni, threeElemSorted, threeElemRevSorted, longerSorted, longerRevSorted, longerUni;

    @BeforeEach
    void setup() {
        oneElem = new int[] {1};
        twoelem = new int[] {4,2};
        threeElemUni = new int[] {1, 3, 2};
        threeElemSorted = new int[] {1, 2, 3};
        threeElemRevSorted = new int[] {3, 2, 1};
        longerSorted = new int[] {2, 5, 6 , 8, 100, 100, 100023};
        longerRevSorted = new int[] {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        longerUni = new int[] {1, 4, 6, 9, 10, 11, 23, 56, 45, 22, 5, 2};
    }

    @Test
    void oneElementArray() {
        assertEquals(1, unimodalArrayMaxElement(oneElem));
    }

    @Test
    void twoElementArray() {
        assertEquals(4, unimodalArrayMaxElement(twoelem));
    }

    @Test
    void threeElementArrays() {
        assertEquals(3, unimodalArrayMaxElement(threeElemUni));
        assertEquals(3, unimodalArrayMaxElement(threeElemSorted));
        assertEquals(3, unimodalArrayMaxElement(threeElemRevSorted));
    }

    @Test
    void longerArrays() {
        assertEquals(100023, unimodalArrayMaxElement(longerSorted));
        assertEquals(10, unimodalArrayMaxElement(longerRevSorted));
        assertEquals(56, unimodalArrayMaxElement(longerUni));
    }
}
