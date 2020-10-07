import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

public class TestAlgorithms {
    @Test
    public void insertSortTest(){
        Integer[] testArray= {3,18,3,6,10,2,4,2,9,1};
        SortingAlgorithms insertSort = new SortingAlgorithms();
        Integer[] sortedArray = insertSort.insertSort(testArray);
        Integer[] correctArray = {1,2,2,3,3,4,6,9,10,18};
        assertArrayEquals(correctArray, sortedArray);
    }

    @Test
    public void insertSortNegativeIntegerTest(){
        Integer[] testArray= {8,2,-3,3,1,20,14,12,9,-1};
        SortingAlgorithms insertSort = new SortingAlgorithms();
        Integer[] sortedArray = insertSort.insertSort(testArray);
        Integer[] correctArray = {-3,-1,1,2,3,8,9,12,14,20};
        assertArrayEquals(correctArray, sortedArray);
    }

    @Test
    public void quickSortTest(){
        Integer [] testArray= {3,18,3,6,10,2,4,2,9,1};
        SortingAlgorithms quickSort = new SortingAlgorithms();
        List<Integer> sortedList = quickSort.quickSort(Arrays.asList(testArray));
        Integer [] correctArray = {1,2,2,3,3,4,6,9,10,18};
        assertIterableEquals(Arrays.asList(correctArray), sortedList);
    }

    @Test
    public void quickSortNegativeIntegerTest(){
        Integer[] testArray= {8,2,-3,3,1,20,14,12,9,-1};
        SortingAlgorithms quickSort = new SortingAlgorithms();
        List<Integer> sortedList = quickSort.quickSort(Arrays.asList(testArray));
        Integer[] correctArray = {-3,-1,1,2,3,8,9,12,14,20};
        assertIterableEquals(Arrays.asList(correctArray), sortedList);
    }
}
