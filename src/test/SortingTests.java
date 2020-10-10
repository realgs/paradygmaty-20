package test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import sorting.InsertionSort;
import sorting.QuickSort;
import sorting.SortingAlgorithm;

import java.util.Arrays;
import java.util.Random;

public class SortingTests {
    private final int ARRAY_NORMAL_SIZE = 10000;
    private final int RANDOM_NUMBER_MAX = 25000;

    private Random random;

    private SortingAlgorithm<Integer> quickSortInt;
    private SortingAlgorithm<Integer> insertionSortInt;

    private SortingAlgorithm<Double> quickSortDouble;
    private SortingAlgorithm<Double> insertionSortDouble;

    @Before
    public void setupAlgorithms() {
        random = new Random();

        quickSortInt = new QuickSort<>();
        quickSortDouble = new QuickSort<>();
        insertionSortInt = new InsertionSort<>();
        insertionSortDouble = new InsertionSort<>();
    }

    @Test
    public void sortAllPositiveValuesIntArray() {
        Integer[] arrayToSort = new Integer[ARRAY_NORMAL_SIZE];

        for (int i = 0; i < ARRAY_NORMAL_SIZE; i++) {
            arrayToSort[i] = random.nextInt(RANDOM_NUMBER_MAX);
        }

        executeIntegerArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortAllNegativeValuesIntArray() {
        Integer[] arrayToSort = new Integer[ARRAY_NORMAL_SIZE];

        for (int i = 0; i < ARRAY_NORMAL_SIZE; i++) {
            arrayToSort[i] = random.nextInt(RANDOM_NUMBER_MAX) * (-1);
        }

        executeIntegerArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortMixedValuesIntArray() {
        Integer[] arrayToSort = new Integer[ARRAY_NORMAL_SIZE];

        for (int i = 0; i < ARRAY_NORMAL_SIZE; i++) {
            arrayToSort[i] = random.nextInt(RANDOM_NUMBER_MAX) * (random.nextBoolean() ? 1 : -1);
        }

        executeIntegerArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortAscendingOrderedIntArray() {
        Integer[] arrayToSort = new Integer[ARRAY_NORMAL_SIZE];

        for (int i = 0; i < ARRAY_NORMAL_SIZE; i++) {
            arrayToSort[i] = i;
        }

        executeIntegerArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortDescendingOrderedIntArray() {
        Integer[] arrayToSort = new Integer[ARRAY_NORMAL_SIZE];

        for (int i = ARRAY_NORMAL_SIZE - 1; i >= 0; i--) {
            arrayToSort[ARRAY_NORMAL_SIZE - 1 - i] = i;
        }

        executeIntegerArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortEmptyIntArray() {
        Integer[] arrayToSort = new Integer[0];

        executeIntegerArrayTest(arrayToSort, 0);
    }

    @Test
    public void sortOneValueIntArray() {
        Integer[] arrayToSort = new Integer[1];

        arrayToSort[0] = random.nextInt(RANDOM_NUMBER_MAX);

        executeIntegerArrayTest(arrayToSort, 1);
    }

    @Test
    public void sortSameValuesDoubleArray() {
        Double[] arrayToSort = new Double[ARRAY_NORMAL_SIZE];

        Arrays.fill(arrayToSort, random.nextDouble() * RANDOM_NUMBER_MAX);

        executeDoubleArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    @Test
    public void sortMixedValuesDoubleArray() {
        Double[] arrayToSort = new Double[ARRAY_NORMAL_SIZE];

        for (int i = 0; i < ARRAY_NORMAL_SIZE; i++) {
            arrayToSort[i] = RANDOM_NUMBER_MAX * random.nextDouble() * (random.nextBoolean() ? 1 : -1);
        }

        executeDoubleArrayTest(arrayToSort, ARRAY_NORMAL_SIZE);
    }

    private void executeIntegerArrayTest(Integer[] arrayToSort, int arrayLength) {
        Integer[] quickSortArray = Arrays.copyOf(arrayToSort, arrayLength);
        Integer[] insertionSortArray = Arrays.copyOf(arrayToSort, arrayLength);
        Integer[] javaSortArray = Arrays.copyOf(arrayToSort, arrayLength);

        quickSortInt.sort(quickSortArray);
        insertionSortInt.sort(insertionSortArray);
        Arrays.sort(javaSortArray);
        Assert.assertArrayEquals(javaSortArray, quickSortArray);
        Assert.assertArrayEquals(javaSortArray, insertionSortArray);
    }

    private void executeDoubleArrayTest(Double[] arrayToSort, int arrayLength) {
        Double[] quickSortArray = Arrays.copyOf(arrayToSort, arrayLength);
        Double[] insertionSortArray = Arrays.copyOf(arrayToSort, arrayLength);
        Double[] javaSortArray = Arrays.copyOf(arrayToSort, arrayLength);

        quickSortDouble.sort(quickSortArray);
        insertionSortDouble.sort(insertionSortArray);
        Arrays.sort(javaSortArray);

        Assert.assertArrayEquals(javaSortArray, quickSortArray);
        Assert.assertArrayEquals(javaSortArray, insertionSortArray);
    }
}