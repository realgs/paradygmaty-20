import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class SortTest
{
    Integer[] generateRandom(int quantity, int minimal, int interval)
    {
        Integer[] array = new Integer[quantity];

        for(int i = 0 ; i < quantity ; i++)
        {
            array[i] = minimal + (int)(Math.random() * interval);
        }

        return array;
    }

    Integer[] arrayUnderTest;
    Integer[] compareArray;
    Sort<Integer> sort;

    @BeforeEach
    void setUp()
    {
        sort = new Sort<>();
    }


    @Test
    void insertionSmallRandomInputTest()
    {
        arrayUnderTest = generateRandom(100,-500,1000);
        compareArray = arrayUnderTest.clone();

        sort.insertionSort(arrayUnderTest, Integer::compare);
        Arrays.sort(compareArray, Integer::compare);

        assertArrayEquals(compareArray,arrayUnderTest);

    }

    @Test
    void insertionBigRandomInputTest()
    {
        arrayUnderTest = generateRandom(10000, - 50000, 100000);
        compareArray = arrayUnderTest.clone();
        sort.insertionSort(arrayUnderTest, Integer::compare);
        Arrays.sort(compareArray, Integer::compare);

        assertArrayEquals(compareArray,arrayUnderTest);

    }

    @Test
    void insertionSortedInputTest()
    {
        arrayUnderTest = new Integer[1000];

        for(int i = 0 ; i < 1000 ; i++)
        {
            arrayUnderTest[i] = i;
        }

        compareArray = arrayUnderTest.clone();

        sort.insertionSort(arrayUnderTest, Integer::compare);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

    @Test
    void insertionReverseSortedInputTest()
    {
        arrayUnderTest = new Integer[1000];

        for(int i = 0 ; i < 1000 ; i++)
        {
            arrayUnderTest[i] = 1000 - i;
        }

        compareArray = arrayUnderTest.clone();

        sort.insertionSort(arrayUnderTest, Integer::compare);
        Arrays.sort(compareArray);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

    @Test
    void quickSmallRandomInputTest()
    {
        arrayUnderTest = generateRandom(1000,-1000,2000);
        compareArray = arrayUnderTest.clone();

        sort.quickSort(arrayUnderTest,Integer::compare);
        Arrays.sort(compareArray);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

    @Test
    void quickBigRandomInputTest()
    {
        arrayUnderTest = generateRandom(100000,-1000000,2000000);
        compareArray = arrayUnderTest.clone();

        sort.quickSort(arrayUnderTest,Integer::compare);
        Arrays.sort(compareArray);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

    @Test
    void quickSortedInputTest()
    {
        arrayUnderTest = new Integer[10000];

        for(int i = 0 ; i < 10000 ; i++)
        {
            arrayUnderTest[i] = i;
        }

        compareArray = arrayUnderTest.clone();

        sort.quickSort(arrayUnderTest,Integer::compare);
        Arrays.sort(compareArray);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

    @Test
    void quickReverseSortedInputTest()
    {
        arrayUnderTest = new Integer[10000];

        for(int i = 0 ; i < 10000 ; i++)
        {
            arrayUnderTest[i] = 10000 - i;
        }

        compareArray = arrayUnderTest.clone();

        sort.quickSort(arrayUnderTest,Integer::compare);
        Arrays.sort(compareArray);

        assertArrayEquals(compareArray,arrayUnderTest);
    }

}