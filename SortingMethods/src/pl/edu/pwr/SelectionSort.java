package pl.edu.pwr;

public class SelectionSort {
    public void sort(int[] array) {
        if(array == null) return;

        int minIdx, n = array.length;
        int tmp;

        for(int i = 0; i < n - 1; ++i) {
            minIdx = i;
            for(int j = i + 1; j < n; ++j) {
                if(array[j] <= array[minIdx]) {
                    minIdx = j;
                }
            }

            tmp = array[i];
            array[i] = array[minIdx];
            array[minIdx] = tmp;

        }
    }

}
