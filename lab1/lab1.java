package lab1;

import java.util.Random;

public class lab1 {

    public static int[] generateArray(int size, int range) {
        int array[] = new int[size];
        Random generator = new Random();
        for (int i = 0; i < array.length; i++) {
            array[i] = generator.nextInt(2 * range) - range; // range: (-range -- range)
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

        for (int i = 1; i <= 5; i++) {

            int range = (int) Math.pow(10, i);

            System.out.println("\nTest " + i + ", range: " + range + ":");

            int[] arr = generateArray(10, range);
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
}
