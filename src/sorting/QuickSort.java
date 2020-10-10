package sorting;

public class QuickSort<T extends Comparable<T>> implements SortingAlgorithm<T> {

    @Override
    public T[] sort(T[] array) {
        if (array.length > 0) {
            quickSort(array, 0, array.length - 1);
        }

        return array;
    }

    public void quickSort(T[] array, int startIndex, int endIndex) {
        int left = startIndex;
        int right = endIndex;
        T temp;
        T pivot = array[(startIndex + endIndex) / 2];

        while (left <= right) {
            while (array[left].compareTo(pivot) < 0) left++;
            while (array[right].compareTo(pivot) > 0) right--;

            if (left <= right) {
                temp = array[left];
                array[left] = array[right];
                array[right] = temp;

                left++;
                right--;
            }
        }

        if (startIndex < right) quickSort(array, startIndex, right);
        if (left < endIndex) quickSort(array, left, endIndex);
    }
}