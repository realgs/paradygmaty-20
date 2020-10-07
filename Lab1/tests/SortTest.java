import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

public class SortTest {
    private int randomSeed = 0;
    private int randomSampleSize = 10000;
    private int[] expected, actual;
    private int[] randomExpected, randomActual;

    @BeforeEach
    public void setUp() {
        expected = new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        actual = new int[] {1, 7, 4, 3, 0, 9, 2, 5, 8, 6};

        Random r = new Random(0);
        randomActual = r.ints(randomSampleSize).toArray();
        randomExpected = new int[randomSampleSize];
        System.arraycopy(randomActual, 0, randomExpected, 0, randomActual.length);
        Arrays.sort(randomExpected);
    }

    @Test
    public void testQuicksortSimple() {
        Sort.quicksort(actual);
        assertArrayEquals(expected, actual);
    }

    @Test
    public void testQuicksortRandomArray() {
        Sort.quicksort(randomActual);
        assertArrayEquals(randomExpected, randomActual);
    }
}
