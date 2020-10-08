import java.util.ArrayList;
import java.util.Random;

public class QuickSort {

    public static <T extends Comparable<T>> void sort(ArrayList<T> list) {
        quickSort(list, 0, list.size() - 1);
    }

    private static <T extends Comparable<T>> void quickSort(ArrayList<T> list, int start, int end) {
        if (start < end) {
            int q = partition(list, start, end);
            quickSort(list, start, q - 1);
            quickSort(list, q + 1, end);
        }
    }

    private static <T extends Comparable<T>> int partition(ArrayList<T> list, int start, int end) {
        int r = new Random().nextInt(end - start) + start;
        swap(list, r, end);
        T x = list.get(end);
        int i = start - 1;
        for (int j = start; j < end; j++) {
            if (x.compareTo(list.get(j)) > 0) {
                swap(list, ++i, j);
            }
        }
        swap(list, i + 1, end);
        return i + 1;
    }


    private static <T> void swap(ArrayList<T> list, int left, int right) {
        T temp = list.get(left);
        list.set(left, list.get(right));
        list.set(right, temp);
    }

}


