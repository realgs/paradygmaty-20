package sorting_methods;

import java.util.List;

public class BubbleSorter<E extends Comparable<E>> implements Sorter<E> {

    /** bubblesort implementation
     * works only for mutable lists */
    @Override
    public void sort(List<E> list) {
        for (int i = 0; i < list.size() - 1; i++) {
            for (int j = 0; j < list.size() - i - 1; j++) {
                if (list.get(j).compareTo(list.get(j + 1)) > 0) swap(list, j, j + 1);
            }
        }
    }

    private void swap(List<E> list, int firstIndex, int secondIndex) {
        E tmp = list.get(firstIndex);
        list.set(firstIndex, list.get(secondIndex));
        list.set(secondIndex, tmp);
    }
}
