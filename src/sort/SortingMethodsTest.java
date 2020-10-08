package sort;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

class SortingMethodsTest {
    private double[] empty;
    private double[] negative;
    private double[] negativeExp;
    private double[] random;
    private double[] randomExp;

    @BeforeEach
    void setUp() {

        Random ran = new Random();
        final int randomArrayLength = 10000;
        random = new double[randomArrayLength];
        for(int i=0;i<randomArrayLength;i++){
            random[i] = ran.nextDouble();
        }
        randomExp = random.clone();
        Arrays.sort(randomExp);

        empty = new double[0];

        negative = new double[] {-34.6,-0.4,-4,-87.9,-127.77,-12.5};
        negativeExp = new double[] {-127.77,-87.9,-34.6,-12.5,-4,-0.4};
    }

    @Test
    void mergeSortEmpty() {
        double[] msEresoult = SortingMethods.mergeSort(empty);
        assertArrayEquals(empty,msEresoult);
    }
    @Test
    void mergeSortNegative() {
        double[] msNresoult = SortingMethods.mergeSort(negative);
        assertArrayEquals(negativeExp,msNresoult);
    }
    @Test
    void mergeSortRandom() {
        double[] msRresoult = SortingMethods.mergeSort(random);
        assertArrayEquals(randomExp,msRresoult);
    }

    @Test
    void insertSortEmpty() {
        double[] isEresoult = SortingMethods.insertSort(empty);
        assertArrayEquals(empty,isEresoult);
    }
    @Test
    void insertSortNegative() {
        double[] isNresoult = SortingMethods.insertSort(negative);
        assertArrayEquals(negativeExp,isNresoult);
    }
    @Test
    void insertSortRandom() {
        double[] isRresoult = SortingMethods.insertSort(random);
        assertArrayEquals(randomExp,isRresoult);
    }
}