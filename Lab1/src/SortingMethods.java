import java.util.*;

public class SortingMethods {

    public static void main(String[] args) {

        testQuicksort();
        testMergesort();
    }

    private static void testQuicksort() {

        testSortingMethod("Quicksort");
    }

    private static void testMergesort() {

        testSortingMethod("Mergesort");
    }

    private static void testSortingMethod(String methodName) {

        List<List<Integer>> testCases = new ArrayList<>(Arrays.asList(

                Arrays.asList(9, 1, 4, 0, 2, 150, -15, 0, 1, -1, -1, -4),
                Arrays.asList(9, 8, 7, 6, 5, 4, 3, 2, 2, 1, 0, -1, -1, -2),
                Arrays.asList(-2, -1, -1, 0, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9),
                Arrays.asList(5, 5, 5, 5, 5, 5, 5, 5),
                Collections.emptyList()
        ));

        System.out.printf("# %s%n", methodName);

        for(List<Integer> testCase : testCases) {

            System.out.println("\nBefore sort: " + testCase);

            if(methodName.equals("Quicksort"))
                quicksort(testCase);
            else if(methodName.equals("Mergesort"))
                testCase = mergesort(testCase);
            else throw new IllegalArgumentException();

            System.out.println("After sort:  " + testCase);
        }

        System.out.println("\n");
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

        if(list.size() == 0)
            return new ArrayList<>();

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
