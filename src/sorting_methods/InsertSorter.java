package sorting_methods;

import java.util.List;

public class InsertSorter<E extends Comparable<E>> implements Sorter<E> {

    /** insertsort implementation
     * works only for mutable lists */
    @Override
    public void sort(List<E> list) {
        for (int i = 1; i < list.size(); i++) {
            E value = list.get(i);
            int j = i - 1;

            while (j >= 0 && list.get(j).compareTo(value) > 0) {
                list.set(j + 1, list.get(j));
                j--;
            }
            list.set(j + 1, value);
        }
    }
}
