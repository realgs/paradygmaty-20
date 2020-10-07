package pl.edu.pwr;

public class SelectionSort {
    public void sort(int[] array) {
        int minInd, tmp, n = array.length;

        for(int i = 0; i < n - 1; ++i) {
            minInd = i;
            for(int j = i + 1; j < n; ++j) {
                if(array[j] < array[minInd]) {
                    minInd = j;
                }
            }

            tmp = array[i];
            array[i] = array[minInd];
            array[minInd] = tmp;

        }
    }
}
