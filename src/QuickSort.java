import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

public class QuickSort<T extends Comparable<T>> {
    private Comparator<T> comparator;

    protected QuickSort(Comparator<T> comparator) {
        this.comparator = comparator;
    }

    public void sort(ArrayList<T> list) {
        quickSort(list, 0, list.size() - 1);
    }

    private void quickSort(ArrayList<T> list, int start, int end) {
        if (start < end) {
            int q = partition(list, start, end);
            quickSort(list, start, q - 1);
            quickSort(list, q + 1, end);
        }
    }

    private int partition(ArrayList<T> list, int start, int end) {
        int r = new Random().nextInt(end - start) + start;
        swap(list, r, end);
        T x = list.get(end);
        int i = start - 1;
        for (int j = start; j < end; j++) {
            if (comparator.compare(list.get(j), x) <= 0) {
                swap(list, ++i, j);
            }
        }
        swap(list, i + 1, end);
        return i + 1;
    }


    private void swap(ArrayList<T> list, int left, int right) {
        T temp = list.get(left);
        list.set(left, list.get(right));
        list.set(right, temp);
    }

}


