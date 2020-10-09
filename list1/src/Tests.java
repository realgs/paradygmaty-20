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
        List<Integer> list = new ArrayList<>(Arrays.asList(a));
        InsertSort.sort(list, Integer::compareTo);
        Collections.sort(assertList);
        Assert.assertEquals(assertList, list);

        list = new ArrayList<>(Arrays.asList(a));
        BubbleSort.sort(list, Integer::compareTo);
        Assert.assertEquals(assertList, list);
    }

    @Test
    void sortTypeDouble() {
        Double[] a = new Double[size];
        for (int i = 0; i < size; i++) {
            a[i] = r.nextDouble() * size;
        }
        List<Double> assertList = new ArrayList<>(Arrays.asList(a));
        List<Double> list = new ArrayList<>(Arrays.asList(a));
        BubbleSort.sort(list, Double::compareTo);
        Collections.sort(assertList);
        Assert.assertEquals(assertList, list);

        list = new ArrayList<>(Arrays.asList(a));
        BubbleSort.sort(list, Double::compareTo);
        Assert.assertEquals(assertList, list);
    }

}
