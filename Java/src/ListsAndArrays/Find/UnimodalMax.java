package ListsAndArrays.Find;

public class UnimodalMax {

    /** Find Max Element of Unimodal Array
     *
     * INPUT: A unimodal array of n distinct elements.
     * @param uni
     *
     * OUTPUT: The maximum element in uni.
     * @return
     */
    static int unimodalArrayMaxElement(int[] uni) {

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
