import java.util.ArrayList;
import java.util.Iterator;
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

        list = new ArrayList<>();

        list.add(9);
        list.add(1);
        list.add(4);
        list.add(2);
        list.add(0);
        list.add(1);
        list.add(-1);
        list.add(-1);
        list.add(-4);

        list = mergesort(list);

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

    public static <T extends Comparable<T>> List<T> mergesort(List<T> list) {

        return mergesort(list, 0, list.size()-1);
    }

    private static <T extends Comparable<T>> List<T> mergesort(List<T> list, int startIndex, int endIndex) {

        if(startIndex == endIndex) {

            List<T> result = new ArrayList<>();

            result.add(list.get(startIndex));

            return result;
        }

        int splitIndex = startIndex + (endIndex - startIndex) / 2;

        List<T> leftPart = mergesort(list, startIndex, splitIndex);
        List<T> rightPart = mergesort(list, splitIndex + 1, endIndex);

        return merge(leftPart, rightPart);
    }

    private static <T extends Comparable<T>> List<T> merge(List<T> left, List<T> right) {

        List<T> result = new ArrayList<>();

        Iterator<T> l = left.iterator();
        Iterator<T> r = right.iterator();
        T elemL = null;
        T elemR = null;

        boolean contL, contR;

        if(contL = l.hasNext())
            elemL = l.next();

        if(contR = r.hasNext())
            elemR = r.next();

        while(contL && contR) {

            if (elemL.compareTo(elemR) <= 0) {

                result.add(elemL);

                if(contL = l.hasNext())
                    elemL = l.next();
                else result.add(elemR);

            } else {

                result.add(elemR);

                if(contR = r.hasNext())
                    elemR = r.next();
                else result.add(elemL);
            }
        }

        while(l.hasNext())
            result.add(l.next());

        while(r.hasNext())
            result.add(r.next());

        return result;
    }
}
