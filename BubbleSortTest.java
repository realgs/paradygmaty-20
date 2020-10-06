package SortingAlorithms;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class BubbleSortTest {
    private BubbleSort bubbleSort;
    private ArrayList<Double> doubleList;
    private ArrayList<Integer> integerList;
    private Random rand;

    @BeforeEach
    void setUp() {
        bubbleSort = new BubbleSort();
        rand = new Random();
        doubleList = new ArrayList<>();
        doubleList.add(6.2);
        doubleList.add(-0.87);
        doubleList.add(1.2);
        doubleList.add(-0.87);
        doubleList.add(13.8);
        doubleList.add(-20.87);
        doubleList.add(1.22);
        doubleList.add(-87.06);
        doubleList.add(120.8);
        doubleList.add(870.8);

        integerList = new ArrayList<>();
        for(int i=0; i<100; i++){
            integerList.add(rand.nextInt());
        }
    }

    @Test
    void sortingIntegersTest() {
        ArrayList<Integer> copiedList = new ArrayList<>(integerList);
        Collections.sort(copiedList);
        assertEquals(bubbleSort.sort(integerList), copiedList);
    }

    @Test
    void sortingDoublesTest() {
        ArrayList<Double> copiedList = new ArrayList<>(doubleList);
        Collections.sort(copiedList);
        assertEquals(bubbleSort.sort(doubleList), copiedList);
    }
}
