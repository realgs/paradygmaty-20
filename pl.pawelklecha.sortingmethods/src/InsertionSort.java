import java.util.List;

public class InsertionSort<T extends Comparable<T>> implements SortingMethod<T> {

    @Override
    public void sort(List<T> list) {
        for (int i = 1; i < list.size(); i++) {
            int j = i;
            T keyValue = list.get(j);

            while (j > 0 && (keyValue.compareTo(list.get(j - 1)) < 0)) {
                list.set(j, list.get(j - 1));
                j--;
            }
            list.set(j, keyValue);
        }

    }
}