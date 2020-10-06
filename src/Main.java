import sorting_methods.BubbleSorter;
import sorting_methods.InsertSorter;
import sorting_methods.Sorter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        System.out.println("BUBBLESORT: ");
        Sorter<Integer> bubbleSorter = new BubbleSorter<>();
        List<Integer> integerList = new ArrayList<>(Arrays.asList(3, 1, -5, 1, 5, -3, 2, 5, -8, 10));
        List<Integer> reversedList = new ArrayList<>(Arrays.asList(10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
        System.out.print(integerList + " -> ");
        bubbleSorter.sort(integerList);
        System.out.println(integerList);
        System.out.print(reversedList + " -> ");
        bubbleSorter.sort(reversedList);
        System.out.println(reversedList);


        System.out.println("___________________");
        System.out.println("INSERTSORT");
        Sorter<Integer> insertSorter = new InsertSorter<>();
        List<Integer> integerList2 = new ArrayList<>(Arrays.asList(3, 1, 5, 1, 5, 3, 2, 5, 8, 10));
        List<Integer> reversedList2 = new ArrayList<>(Arrays.asList(10, 9, 8, 7, 6, 5, 4, 3, 2, 1));
        System.out.print(integerList2 + " -> ");
        insertSorter.sort(integerList2);
        System.out.println(integerList2);
        System.out.print(reversedList2 + " -> ");
        insertSorter.sort(reversedList2);
        System.out.println(reversedList2);
    }
}
