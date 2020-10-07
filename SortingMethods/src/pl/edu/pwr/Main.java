package pl.edu.pwr;

import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        SelectionSort sort = new SelectionSort();
        int[] arr = {-14, 28, 13, 4, 27, -122, 1, 53};
        sort.sort(arr);
        System.out.println(Arrays.toString(arr));
    }
}
