import java.util.ArrayList;

public class BubbleSort {
    public static void sort(ArrayList<Integer> list) {
        for (int i = 0; i < list.size(); i++) {
            for (int j = 0; j < list.size()-1 - i; j++) {
                int prev = list.get(j);
                int next = list.get(j + 1);
                if (prev > next) {
                    list.set(j + 1, prev);
                    list.set(j, next);
                }
            }
        }
    }
}
