import java.util.ArrayList;
import java.util.Random;

public class QuickSort {
    public <T extends Comparable<? super T>> void sort(ArrayList<T> lista) {
        quickSort(lista, 0, lista.size() - 1);
    }

    public <T extends Comparable<? super T>> void quickSort(ArrayList<T> lista, int leftIndex, int rightIndex) {
        if (rightIndex - leftIndex > 0) {
            int splitIndex = pivotIndex(lista, leftIndex, rightIndex);
            quickSort(lista, leftIndex, splitIndex - 1);
            quickSort(lista, splitIndex + 1, rightIndex);
        }
    }

    private <T extends Comparable<? super T>> int pivotIndex(ArrayList<T> lista, int start, int end) {

        int rand = new Random().nextInt(end - start + 1) + start;
        swap(lista, start, rand);
        T pivot = lista.get(start);

        int searchForBigger = start + 1;
        int searchForLower = end;

        do {
            while (searchForBigger < searchForLower && lista.get(searchForBigger).compareTo(pivot) <= 0) {
                searchForBigger++;
            }
            while (lista.get(searchForLower).compareTo(pivot) > 0)
            {
                searchForLower--;
            }
            if (searchForBigger < searchForLower) {
                swap(lista, searchForLower, searchForBigger);
            }
        }
        while (searchForBigger < searchForLower);
        swap(lista, searchForLower, start);
        return searchForLower;
    }

    private <T> void swap(ArrayList<T> lista, int left, int right) {
        if (left != right) {
            T tmp = lista.get(left);
            lista.set(left, lista.get(right));
            lista.set(right, tmp);
        }
    }
}
