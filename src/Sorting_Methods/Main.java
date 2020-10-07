package Sorting_Methods;

import java.util.Random;

import static Sorting_Methods.Sorting.bubbleSort;
import static Sorting_Methods.Sorting.printArray;

public class Main {
    public static void main(String[] args) {
        System.out.println("Test of first array");
        int[] FirstTestArray = {35, 25, 15, 85,};
        int[] firstSortedArray = bubbleSort(FirstTestArray);
        printArray(firstSortedArray);

        System.out.println("Test of second array");
        int[] secondTestArray = {12, 67, -90, -1000, 0, 135, -80};
        int[] secondSortedArray = bubbleSort(secondTestArray);
        printArray(secondSortedArray);

        System.out.println("Test of random array");
        int[] randomlyGeneratedArray = new int[1000];
        Random r = new Random();
        for (int i = 0; i < randomlyGeneratedArray.length; i++) {
            randomlyGeneratedArray[i] = r.nextInt((500 + 500) + 1) - 500;
        }
        int[] thirdSortedArray = bubbleSort(randomlyGeneratedArray);
        printArray(thirdSortedArray);

    }
}
