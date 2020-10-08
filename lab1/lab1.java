package lab1;

import java.util.Random;

public class lab1 {

    public static int[] generateArray(int size) {
        int array[] = new int[size];
        Random generator = new Random();
        for (int i = 0; i < array.length; i++) {
            array[i] = generator.nextInt(2000) - 1000; // range: (-1000 -- 1000)
        }
        return array;
    }

    public static void printArray(int array[]) {
        for (int i = 0; i < array.length; i++) {
            if (i + 1 == array.length) {
                System.out.print(array[i]);
            } else {
                System.out.print(array[i] + ", ");
            }

        }
        System.out.println("");
    }

    public static void main(String[] args) {

        int[] arr = generateArray(10);
        int[] arr2 = arr.clone();

        System.out.println("\nUnsorted array:");
        printArray(arr);

        System.out.println("\nSorted by mergesort:");
        Sort.mergesort(arr);
        printArray(arr);

        System.out.println("\nSorted by quicksort:");
        Sort.quicksort(arr2, 0, arr2.length - 1);
        printArray(arr2);

    }
}
