import java.util.Comparator;
import java.util.Random;
import java.util.function.Supplier;

public class Tests
{
    private <T> boolean isSorted(T[] arr, Comparator<T> comp) {
        for (int i = 0; i < arr.length - 1; i++) {
            if (comp.compare(arr[i], arr[i + 1]) > 0) {
                return false;
            }
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private <T> Object[] generateArray(int size, Supplier<T> supplier) {
        T[] arr = (T[]) new Object[size];
        for (int i = 0; i < size; i++) {
            arr[i] = supplier.get();
        }
        return arr;
    }

    private Integer[] generateArrayOfIntegers(int size) {
        Random r = new Random();
        Integer[] arr = new Integer[size];
        int i = 0;
        for (Object o : generateArray(size, r::nextInt)) {
            arr[i++] = (Integer) o;
        }
        return arr;
    }

    private Double[] generateArrayOfDoubles(int size) {
        Random r = new Random();
        Double[] arr = new Double[size];
        int i = 0;
        for (Object o : generateArray(size, () -> r.nextDouble() * 10_000 - 5_000)) {
            arr[i++] = (Double) o;
        }
        return arr;
    }

    public void run() {
        testIntegers();
        testDoubles();
    }

    private void testIntegers() {
        int size = 10_000;

        Integer[] arr = generateArrayOfIntegers(size);
        MergeSort.sort(arr, Integer::compareTo);
        System.out.println(isSorted(arr, Integer::compareTo));

        arr = generateArrayOfIntegers(size);
        InsertionSort.sort(arr, Integer::compareTo);
        System.out.println(isSorted(arr, Integer::compareTo));
    }

    private void testDoubles() {
        int size = 10_000;

        Double[] arr = generateArrayOfDoubles(size);
        MergeSort.sort(arr, Double::compareTo);
        System.out.println(isSorted(arr, Double::compareTo));

        arr = generateArrayOfDoubles(size);
        InsertionSort.sort(arr, Double::compareTo);
        System.out.println(isSorted(arr, Double::compareTo));
    }
}
