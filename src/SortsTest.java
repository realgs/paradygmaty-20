import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class SortsTest {
    private Random random = new Random();

    @Test
    public void randomInteger_Test() {
        ArrayList<Integer> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add(random.nextInt());
        }
        ArrayList<Integer> list1 = new ArrayList<>(list);
        ArrayList<Integer> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Integer::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }

    @Test
    public void randomDouble_Test() {
        ArrayList<Double> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add(random.nextDouble());
        }
        ArrayList<Double> list1 = new ArrayList<>(list);
        ArrayList<Double> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Double::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }

    @Test
    public void randomFloat_Test() {
        ArrayList<Float> list = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            list.add(random.nextFloat());
        }
        ArrayList<Float> list1 = new ArrayList<>(list);
        ArrayList<Float> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Float::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }

    @Test
    public void randomByte_Test() {
        ArrayList<Byte> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add(((Integer) random.nextInt()).byteValue());
        }
        ArrayList<Byte> list1 = new ArrayList<>(list);
        ArrayList<Byte> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Byte::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }

    @Test
    public void theSameValue_Test() {
        ArrayList<Integer> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add(1);
        }
        ArrayList<Integer> list1 = new ArrayList<>(list);
        ArrayList<Integer> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Integer::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }

    @Test
    public void negativeValues_Test() {
        ArrayList<Integer> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add(random.nextInt(1000) * -1);
        }
        ArrayList<Integer> list1 = new ArrayList<>(list);
        ArrayList<Integer> list2 = new ArrayList<>(list);
        QuickSort.sort(list);
        InsertSort.sort(list2);
        list1.sort(Integer::compareTo);

        assertTrue(ifSorted(list));
        assertTrue(ifSorted(list1));
        assertTrue(ifSorted(list2));
    }


    public <T extends Comparable<T>> boolean ifSorted(ArrayList<T> list) {
        for (int i = 1; i < list.size(); i++) {
            if (list.get(i - 1).compareTo(list.get(i)) > 0) {
                return false;
            }
        }
        return true;
    }
}
