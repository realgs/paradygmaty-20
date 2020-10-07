import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SortTest {
    private int[] expected, actual;

    @BeforeEach
    public void setUp() {
        expected = new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        actual = new int[] {1, 7, 4, 3, 0, 9, 2, 5, 8, 6};
    }

    @Test
    public void testQuicksortSimple() {
        Sort.quicksort(actual);
        assertArrayEquals(expected, actual);
    }
}
