import java.util.List;

public interface SortingMethod<T extends Comparable<T>> {
    void sort(List<T> list);
}
