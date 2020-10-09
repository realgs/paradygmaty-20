import java.util.Comparator;
import java.util.List;

public class InsertSort {

    public static <T> void sort(List<T> list, Comparator<T> comparator) {
        for (int i = 1; i < list.size(); ++i) {
            T value = list.get(i);
            int j;
            for (j = i; j > 0 && comparator.compare(value, list.get(j - 1)) < 0; --j)
                list.set(j, list.get(j - 1));
            list.set(j, value);
        }
    }
}
