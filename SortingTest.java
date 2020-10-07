import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import org.junit.jupiter.api.BeforeEach;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SortingTest{
    private QuickSort quickSort = new QuickSort();
    private BubbleSort bubbleSort = new BubbleSort();
    private ArrayList<Integer> list = new ArrayList<>();
    private ArrayList<Integer> listBucket;
    private ArrayList<Integer> listQuick;
    private ArrayList<Integer> expected;

    @BeforeEach
    void setUp() {
        list.clear();
    }

    @Test
    void sameElementsSmallTest() {
        for(int i=0;i<10;i++)
            list.add(10);
        listBucket = newList(list);
        listQuick = newList(list);
        expected = newList(list);

        bubbleSort.sort(listBucket);
        quickSort.sort(listQuick);
        Collections.sort(expected);

        assertEquals(expected,listBucket);
        assertEquals(expected,listQuick);
    }
    @Test
    void sameElementsBigTest() {
        for(int i=0;i<10000;i++)
            list.add(10);
        listBucket = newList(list);
        listQuick = newList(list);
        expected = newList(list);

        bubbleSort.sort(listBucket);
        quickSort.sort(listQuick);
        Collections.sort(expected);

        assertEquals(expected,listBucket);
        assertEquals(expected,listQuick);
    }
    @Test
    void randElementsSmallTest() {
        Random rand = new Random();
        for(int i=0;i<10;i++)
            list.add(rand.nextInt(100));
        listBucket = newList(list);
        listQuick = newList(list);
        expected = newList(list);

        bubbleSort.sort(listBucket);
        quickSort.sort(listQuick);
        Collections.sort(expected);

        assertEquals(expected,listBucket);
        assertEquals(expected,listQuick);
    }
    @Test
    void randElementsBigTest() {
        Random rand = new Random();
        for(int i=0;i<10000;i++)
            list.add(rand.nextInt(100000));
        listBucket = newList(list);
        listQuick = newList(list);
        expected = newList(list);

        bubbleSort.sort(listBucket);
        quickSort.sort(listQuick);
        Collections.sort(expected);

        assertEquals(expected,listBucket);
        assertEquals(expected,listQuick);

    }
    @Test
    void nullElementTest() {
        list.add(3);
        list.add(null);
        list.add(1);

        assertThrows(NullPointerException.class,()->bubbleSort.sort(list));
        assertThrows(NullPointerException.class,()->quickSort.sort(list));
    }
    private ArrayList<Integer> newList(ArrayList<Integer> list){
        ArrayList<Integer> newList = new ArrayList<>();
        for(int i=0;i<list.size();i++)
            newList.add(list.get(i));
        return newList;
    }
}
