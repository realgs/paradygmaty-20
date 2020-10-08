package sort;

public class SortingMethods {

    public static double[] mergeSort(double[] tab) {
        mergeSort(tab,0,tab.length-1);
        return tab;
    }
    private static void mergeSort(double[] tab, int startIndex, int endIndex) {
        if(startIndex < endIndex) {
            int splitIndex = startIndex + (endIndex - startIndex)/2;
            mergeSort(tab,startIndex,splitIndex);
            mergeSort(tab, splitIndex+1,endIndex);
            merge(tab,startIndex,splitIndex,endIndex);
        }
    }
    private static void merge(double[] tab, int startIndex, int splitIndex, int endIndex) {
        int n1 = splitIndex - startIndex + 1;
        int n2 = endIndex - splitIndex;

        double[] L = new double [n1];
        double[] R = new double [n2];

        for (int i=0; i<n1; ++i)
            L[i] = tab[startIndex + i];
        for (int j=0; j<n2; ++j)
            R[j] = tab[splitIndex + 1 + j];
        int i = 0, j = 0;

        int k = startIndex;
        while (i < n1 && j < n2)
        {
            if (L[i] <= R[j])
            {
                tab[k] = L[i];
                i++;
            }
            else
            {
                tab[k] = R[j];
                j++;
            }
            k++;
        }
        while (i < n1)
        {
            tab[k] = L[i];
            i++;
            k++;
        }
        while (j < n2)
        {
            tab[k] = R[j];
            j++;
            k++;
        }
    }

    public static double[] insertSort(double[] tab) {
        for (int i = 1; i < tab.length; ++i) {
            double key = tab[i];
            int j = i - 1;
            while (j >= 0 && tab[j] > key) {
                tab[j + 1] = tab[j];
                j = j - 1;
            }
            tab[j + 1] = key;
        }
        return tab;
    }
}
