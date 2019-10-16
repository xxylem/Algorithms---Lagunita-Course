package ListsAndArrays.Find;

public class UnimodalMax {

    public static void main(String[] args) {

        // Examples of unimodal arrays.
        int[] uni = {1, 4, 6, 9, 10, 11, 23, 56, 45, 22, 5, 2};
        System.out.println(unimodalArrayMaxElement(uni));
        int[] uni2 = {1, 3, 2};
        System.out.println(unimodalArrayMaxElement(uni2));
        int[] uni3 = {1};
        System.out.println(unimodalArrayMaxElement(uni3));
        int[] uni4 = {4,2};
        System.out.println(unimodalArrayMaxElement(uni4));
        int[] uni5 = {2, 5, 6 , 8, 100, 100, 100023};
        System.out.println(unimodalArrayMaxElement(uni5));
        int[] uni6 = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        System.out.println(unimodalArrayMaxElement(uni6));
        int[] uni7 = {1, 2, 3};
        System.out.println(unimodalArrayMaxElement(uni7));
        int[] uni8 = {3, 2, 1};
        System.out.println(unimodalArrayMaxElement(uni8));
    }

    /** Find Max Element of Unimodal Array
     *
     * INPUT: A unimodal array of n distinct elements.
     * @param uni
     *
     * OUTPUT: The maximum element in uni.
     * @return
     */
    private static int unimodalArrayMaxElement(int[] uni) {

        int n = uni.length;

        // Establish a 'search window'.
        // We only consider values inside this window to be potentially the max element.
        int windowStart = 0;
        int windowEnd = n - 1;

        // Base case (n=1).
        // The single element of a single element array is the max element.
        if (n == 1) {
            return uni[0];
        }

        // Base case (n=2)
        // 'Manually' compute the max.
        else if (n == 2) {
            return Math.max(uni[0], uni[1]);
        }
        else {
            // Choose two consecutive elements in the middle of the 'search window'.
            int midPoint1 = n / 2;
            int midPoint2 = midPoint1 + 1;

            // If the window start and end are the same, the window size is one element.
            // That must be the max element.
            while (Math.abs(windowEnd - windowStart) > 0) {

                // Of the two elements in the middle, if the left is smaller,
                //  the max element must be in the right half of the 'search window'.
                if (uni[midPoint1] < uni[midPoint2]) {
                    // Shrink the search window to only the right half.
                    windowStart = midPoint2;
                }
                else {
                    // Otherwise, the max element must be in the left half.
                    windowEnd = midPoint1;
                }
                // Calculate new midpoints for the updated search window.
                midPoint1 = (windowEnd + windowStart) / 2;
                midPoint2 = midPoint1 + 1;
            }
            // Since the window is now just one element, it is the max element.
            return uni[windowStart];
        }
    }
}
