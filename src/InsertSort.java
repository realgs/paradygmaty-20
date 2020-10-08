import java.util.ArrayList;

public class InsertSort {

    public static <T extends Comparable<T>> void sort(ArrayList<T> list) {
        T value;
        for (int i = 1; i < list.size(); i++) {
            value = list.get(i);
            int j = i;
            while (j > 0 && value.compareTo(list.get(j - 1)) < 0) {
                list.set(j, list.get(j - 1));
                j--;
            }
            list.set(j, value);
        }

    }
}
