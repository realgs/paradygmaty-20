package SortingAlorithms;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SelectSortTest {
    private SelectSort selectSort;
    private ArrayList<Double> doubleList;
    private ArrayList<Integer> integerList;
    private Random rand;

    @BeforeEach
    void setUp() {
        selectSort = new SelectSort();
        rand = new Random();
        doubleList = new ArrayList<>();
        for(int i=0; i<50; i++){
            doubleList.add(rand.nextDouble()*100);
        }

        integerList = new ArrayList<>();
        for(int i=0; i<100; i++){
            integerList.add(rand.nextInt());
        }
    }

    @Test
    void sortingIntegersTest() {
        ArrayList<Integer> copiedList = new ArrayList<>(integerList);
        Collections.sort(copiedList);
        assertEquals(copiedList, selectSort.sort(integerList));
    }

    @Test
    void sortingDoublesTest() {
        ArrayList<Double> copiedList = new ArrayList<>(doubleList);
        Collections.sort(copiedList);
        assertEquals(copiedList, selectSort.sort(doubleList));
    }
}
