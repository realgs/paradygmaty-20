import org.junit.Assert;
import org.junit.jupiter.api.Test;

import java.util.*;


public class Tests {
    private Random r = new Random();
    private int size = 1000;

    @Test
    void sortTypeInteger() {
        Integer[] a = new Integer[size];
        for (int i = 0; i < size; i++) {
            a[i] = r.nextInt(Integer.MAX_VALUE);
        }
        List<Integer> assertList = new ArrayList<>(Arrays.asList(a));
        List<Integer> integerList = new ArrayList<>(Arrays.asList(a));
        InsertSort.sort(integerList, Integer::compareTo);
        Collections.sort(assertList);
        Assert.assertEquals(assertList, integerList);
    }

    @Test
    void sortTypeDouble() {
        Double[] a = new Double[size];
        for (int i = 0; i < size; i++) {
            a[i] = r.nextDouble() * size;
        }
        List<Double> assertList = new ArrayList<>(Arrays.asList(a));
        List<Double> doubleList = new ArrayList<>(Arrays.asList(a));
        BubbleSort.sort(doubleList, Double::compareTo);
        Collections.sort(assertList);
        Assert.assertEquals(assertList, doubleList);
    }

}
