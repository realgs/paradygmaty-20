package sorting;

public class InsertionSort<T extends Comparable<T>> implements SortingAlgorithm<T> {

    @Override
    public T[] sort(T[] array) {
        T value;
        int j;

        for (int i = 1; i < array.length; i++) {
            j = i;
            value = array[j];

            while (j > 0 && (value.compareTo(array[j - 1]) < 0)) {
                array[j] = array[j - 1];
                j--;
            }
            array[j] = value;
        }

        return array;
    }
}