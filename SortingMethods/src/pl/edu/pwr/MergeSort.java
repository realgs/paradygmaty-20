package pl.edu.pwr;

import java.util.Arrays;

public class MergeSort {
    public void sort(int[] array) {
        if(array == null) return;
        mergeSort(array, 0, array.length - 1);
    }

    private void mergeSort(int[] array, int fst, int lst) {
        if(fst < lst) {
            int mid = (fst + lst) / 2;
            mergeSort(array, fst, mid);
            mergeSort(array, mid+1, lst);
            merge(array, fst, mid, lst);
        }
    }

    private void merge(int[] array, int fst, int mid, int lst) {
        int[] l = new int[mid - fst + 1];
        int[] r = new int[lst - mid];

        for (int i = 0; i < l.length; i++) {
            l[i] = array[fst + i];
        }
        for (int i = 0; i < r.length; i++) {
            r[i] = array[mid + 1 + i];
        }

        int i = 0, j = 0, k = fst;
        while (i < l.length && j < r.length) {
            if (l[i] <= r[j]) {
                array[k++] = l[i++];
            }
            else {
                array[k++] = r[j++];
            }
        }

        while (i < l.length) {
            array[k++] = l[i++];
        }
        while (j < r.length) {
            array[k++] = r[j++];
        }

    }
}
