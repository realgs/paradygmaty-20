import org.junit.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

public class TestAlgorithms {
    @Test
    public void insertSortTest(){
        int[] testArray= {3,18,3,6,10,2,4,2,9,1};
        SortingAlgorithms insertSort = new SortingAlgorithms();
        int[] sortedArray = insertSort.insertSort(testArray);
        int[] correctArray = {1,2,2,3,3,4,6,9,10,18};
        assertArrayEquals(correctArray, sortedArray);
    }
}
