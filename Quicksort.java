
public class Quicksort {

    private int tab[];
    private int length;

    private void swaper(int a, int b) {
        int temp = tab[a];
        tab[a] = tab[b];
        tab[b] = temp;
    }

    public void sort(int[] temp_tab) {


        this.tab = temp_tab;
        length = temp_tab.length;
        quickSort(0, length - 1);
    }

    private void quickSort(int lowerIndex, int higherIndex) {

        int i = lowerIndex;
        int j = higherIndex;

        int pivot = tab[lowerIndex+(higherIndex-lowerIndex)/2];

        while (i <= j) {

            while (tab[i] < pivot) {
                i++;
            }
            while (tab[j] > pivot) {
                j--;
            }
            if (i <= j) {
                swaper(i, j);
                i++;
                j--;
            }
        }

        if (lowerIndex < j)
            quickSort(lowerIndex, j);
        if (i < higherIndex)
            quickSort(i, higherIndex);
    }

}
