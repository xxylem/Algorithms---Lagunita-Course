package Chapter5;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Random;

import static Chapter5.QuickSort.quickSort;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class QuickSortTest {

    private int[] a, b, c, d, e, f, g, h, i, j;
    private int[] aS, bS, cS, dS, eS, fS, gS, hS, iS, jS;

    @BeforeEach
    void setup() {
        // Small example arrays to check
        a = new int[] {1} ;
        b = new int[] {1, 2};
        c = new int[] {2, 1};
        d = new int[] {1, 2, 3};
        e = new int[] {1, 3, 2};
        f = new int[] {2, 1, 3};
        g = new int[] {2, 3, 1};
        h = new int[] {3, 1, 2};
        i = new int[] {3, 2, 1};
        j = new int[] {5, 6, 8, 9, 30, 2, 1};

        // Copies of the above example arrays
        // Sorted by inbuilt library function.
        aS = sortCopy(a);
        bS = sortCopy(b);
        cS = sortCopy(c);
        dS = sortCopy(d);
        eS = sortCopy(e);
        fS = sortCopy(f);
        gS = sortCopy(g);
        hS = sortCopy(h);
        iS = sortCopy(i);
        jS = sortCopy(j);
    }

    @Test
    void testSmallArrays() {
        // Use quickSort to sort all the example arrays.
        quickSort(a);
        quickSort(b);
        quickSort(c);
        quickSort(d);
        quickSort(e);
        quickSort(f);
        quickSort(g);
        quickSort(h);
        quickSort(i);
        quickSort(j);

        // Check that they are the same as
        // the library-sorted copies.
        assertArrayEquals(aS, a);
        assertArrayEquals(bS, b);
        assertArrayEquals(cS, c);
        assertArrayEquals(dS, d);
        assertArrayEquals(eS, e);
        assertArrayEquals(fS, f);
        assertArrayEquals(gS, g);
        assertArrayEquals(hS, h);
        assertArrayEquals(iS, i);
        assertArrayEquals(jS, j);
    }

    @Test
    void extendedRandomisedQuickSortTest() {

        Random r = new Random();

        // Test arrays from length 0 to 999
        for (int i = 0; i < 1000; i++) {

            // Initialise a new array of size i
            int[] arr = new int[i];

            // Fill the array with random integers.
            for (int j = 0; j < i; j++) {
                arr[j] = r.nextInt();
            }

            // Sort the array separately using quickSort implementation
            // and library sort.
            int[] arrSortedCopy = sortCopy(arr);
            quickSort(arr);

            // Compare the sorts.
            assertArrayEquals(arrSortedCopy, arr);
        }
    }

    /** Input: an n-element integer array @arr
     *  Output: a sorted copy of the array.
     *  Does not modify the input array.
     * */
    private static int[] sortCopy(int[] arr) {
        int[] arrCopy = Arrays.copyOf(arr, arr.length);
        Arrays.sort(arrCopy);
        return arrCopy;
    }

}
