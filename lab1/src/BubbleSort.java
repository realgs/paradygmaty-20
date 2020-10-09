import java.util.Comparator;
import java.util.List;

public class BubbleSort {

    public static <T> void sort(List<T> list, Comparator<T> comparator){
        int size = list.size();
        for (int i = 1; i < size; ++i) {
            for (int left = 0; left < (size - i); ++left) {
                int right = left + 1;
                if (comparator.compare(list.get(left), list.get(right)) > 0)
                    swap(list, left, right);
            }
        }
    }

    private static <T> void swap(List<T> list, int left, int right){
        T temp = list.get(left);
        list.set(left, list.get(right));
        list.set(right, temp);
    }
}
