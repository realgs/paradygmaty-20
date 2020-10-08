package pl.edu.pwr;

import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        MergeSort mSort = new MergeSort();
        SelectionSort sSort = new SelectionSort();


        int[] intArr = {-14, 28, 13, 4, 27, -122, 1, 53, 8, 3, 14, 13},
                intArrCopy = new int[intArr.length];
        int[] bigIntArr = {4323243, -2315555, 1236997, -1324578, 69994312},
                bigIntArrCopy = new int[bigIntArr.length];
//        float[] floatArr = {1.1f, 5.28f, -3.42f, 1356.55f};


        testSort(mSort, sSort, intArr, intArrCopy);
        testSort(mSort, sSort, bigIntArr, bigIntArrCopy);

        mSort.sort(null);
        sSort.sort(null);

//        mSort.sort(floatArr);
//        sSort.sort(floatArr);
    }

    private static void testSort(MergeSort mSort, SelectionSort sSort, int[] arr, int[] arrCopy) {
        System.arraycopy(arr, 0, arrCopy, 0, arr.length);
        mSort.sort(arrCopy);
        System.out.println(Arrays.toString(arrCopy) + " - merge sort");

        System.arraycopy(arr, 0, arrCopy, 0, arr.length);
        sSort.sort(arrCopy);
        System.out.println(Arrays.toString(arrCopy) + " - selection sort");

        System.arraycopy(arr, 0, arrCopy, 0, arr.length);
        Arrays.sort(arrCopy);
        System.out.println(Arrays.toString(arrCopy) + " - Arrays.sort()\n");
    }
}
