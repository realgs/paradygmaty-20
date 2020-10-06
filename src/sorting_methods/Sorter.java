package sorting_methods;

import java.util.List;
public interface Sorter<E extends Comparable<E>> {
    void sort(List<E> list);
}
