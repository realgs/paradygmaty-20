package Sorting_Methods;

import java.util.Random;

import static Sorting_Methods.Sorting.*;

public class Main {
    public static void main(String[] args) {
        System.out.println("Test of first array");
        int[] firstTestArray = {35, 25, 15, 85,};
        System.out.println("Bubble sort");
        int[] firstSortedArrayBubble = bubbleSort(firstTestArray);
        printArray(firstSortedArrayBubble);
        System.out.println("Selection sort");
        int[] firstSortedArraySelection = selectionSort(firstTestArray);
        printArray(firstSortedArraySelection);

        System.out.println("Test of second array");
        int[] secondTestArray = {12, 67, -90, -1000, 0, 135, -80};
        System.out.println("Bubble sort");
        int[] secondSortedArrayBubble = bubbleSort(secondTestArray);
        printArray(secondSortedArrayBubble);
        System.out.println("Selection sort");
        int[] secondSortedArraySelection = selectionSort(secondTestArray);
        printArray(secondSortedArraySelection);

        System.out.println("Test of random generated array");
        int[] randomGeneratedArray = new int[1000];
        Random r = new Random();
        for (int i = 0; i < randomGeneratedArray.length; i++) {
            randomGeneratedArray[i] = r.nextInt((500 + 500) + 1) - 500;
        }
        System.out.println("Bubble sort");
        int[] thirdSortedArrayBubble = bubbleSort(randomGeneratedArray);
        printArray(thirdSortedArrayBubble);
        System.out.println("Selection sort");
        int[] thirdSortedArraySelection = selectionSort(randomGeneratedArray);
        printArray(thirdSortedArraySelection);
    }
}
