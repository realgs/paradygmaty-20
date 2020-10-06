package lab1;

public class Sort {

    public static void swap(int[] arr, int i, int j) {
        int t = arr[i];
        arr[i] = arr[j];
        arr[j] = t;
    }

    private static void merge(int array[], int start, int middle, int end) {
        int[] res = new int[end - start + 1];
        int resIndex = 0;
        int aIndex = start, bIndex = middle + 1;
        while (aIndex <= middle && bIndex <= end) {
            while (aIndex <= middle && bIndex <= end && array[aIndex] < array[bIndex]) {
                res[resIndex++] = array[aIndex++];

            }
            while (aIndex <= middle && bIndex <= end && array[aIndex] >= array[bIndex]) {
                res[resIndex++] = array[bIndex++];
            }
        }

        while (aIndex <= middle) {
            res[resIndex++] = array[aIndex++];
        }

        while (bIndex <= end) {
            res[resIndex++] = array[bIndex++];
        }

        for (int i = 0; i < res.length; i++) {
            array[i + start] = res[i];
        }

    }

    public static void mergesort(int array[]) {
        int start = 0;
        int end = array.length - 1;
        int middle = (end + start) / 2;

        mergesort(array, start, middle);
        mergesort(array, middle + 1, end);

        merge(array, start, middle, end);
    }

    private static void mergesort(int array[], int start, int end) {
        if (start < end) {
            int middle = (end + start) / 2;

            mergesort(array, start, middle);
            mergesort(array, middle + 1, end);

            merge(array, start, middle, end);
        }
    }

    public static void quicksort(int array[], int start, int end) {
        if (start < end) {
            int left = start, right = end;
            int pivot = array[start + (end - start) / 2];

            while (left <= right) {
                while (array[left] < pivot) {
                    left++;
                }
                while (array[right] > pivot) {
                    right--;
                }
                if (left <= right) {
                    swap(array, left, right);
                    left++;
                    right--;
                }
            }
            if (start < right)
                quicksort(array, start, right);
            if (left < end)
                quicksort(array, left, end);
        }
    }
}
