import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class SortingMethods {

    public static void main(String[] args) {

        List<Integer> list = new ArrayList<>();

        list.add(9);
        list.add(1);
        list.add(4);
        list.add(2);
        list.add(0);
        list.add(1);
        list.add(-1);
        list.add(-1);
        list.add(-4);

        quicksort(list);

        System.out.println(list);
    }

    public static <T extends Comparable<T>> void quicksort(List<T> list) {

        quicksort(list, 0, list.size());
    }

    private static <T extends Comparable<T>> void quicksort(List<T> list, int startIndex, int endIndex) {

        if (endIndex - startIndex > 1) {

            int partition = partition(list, startIndex, endIndex);

            quicksort(list, startIndex, partition );
            quicksort(list, partition+1, endIndex);
        }
    }

    private static <T extends Comparable<T>> int partition(List<T> list, int nFrom, int nTo) {

        Random random = new Random();
        int rnd = nFrom + random.nextInt(nTo-nFrom);

        swap(list, nFrom, rnd);

        T value=list.get(nFrom);

        int idxBigger = nFrom + 1, idxLower= nTo - 1;

        do{

            while(idxBigger <= idxLower && list.get(idxBigger).compareTo(value) <= 0)
                idxBigger++;

            while(list.get(idxLower).compareTo(value) > 0)
                idxLower--;

            if(idxBigger < idxLower)
                swap(list, idxBigger, idxLower);
        }

        while(idxBigger < idxLower);

        swap(list, idxLower, nFrom);

        return idxLower;
    }

    private static <T> void swap(List<T> list, int left, int right) {

        if (left != right) {

            T temp = list.get(left);

            list.set(left, list.get(right));

            list.set(right, temp);
        }
    }
}
