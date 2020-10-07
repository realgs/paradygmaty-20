package Sorting_Methods;

public class Sorting {

    public static int[] bubbleSort(int[] array) {
        int[] sortingArray = array.clone();
        for (int i = 0; i < sortingArray.length; i++) {
            for (int j = 0; j < sortingArray.length - i - 1; j++) {
                if (sortingArray[j] > sortingArray[j + 1]) {
                    swap(sortingArray, j, (j + 1));
                }
            }
        }
        return sortingArray;
    }

    public static void swap(int[] array, int firstIndex, int secondIndex) {
        int temp = array[firstIndex];
        array[firstIndex] = array[secondIndex];
        array[secondIndex] = temp;
    }

    public static void printArray(int[] array) {
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + " ");
        }
        System.out.println();
    }
}
