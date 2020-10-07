import java.util.Collections;
import java.util.List;
import java.util.Random;

public class Sort {

    public List<Integer> bubbleSort(List<Integer> list) throws NullPointerException{
        if(list == null)
            throw new IllegalArgumentException();

        if(list.isEmpty()) {
            return list;
        }

        int temp;
        for(int i=0; i < list.size()-1; i++) {
            for(int j=0; j < list.size()-1-i; j++) {
                if(list.get(j) > list.get(j+1)) {
                    temp = list.get(j);
                    list.set(j, list.get(j+1));
                    list.set(j+1, temp);
                }
            }
        }

        return list;
    }

    public List<Integer> quickSort(List<Integer> list) {
        if(list == null)
            throw new IllegalArgumentException();

        if(list.isEmpty()) {
            return list;
        }
        sort(list, 0, list.size());
        return list;
    }

    private void sort(List<Integer> list, int startIndex, int endIndex) {
        if (endIndex - startIndex > 1) {
            int partition = partition(list, startIndex, endIndex);
            sort(list, startIndex, partition );
            sort(list, partition + 1, endIndex);
        }
    }

    private int partition(List<Integer> list, int nFrom, int nTo) {

        Random rnd = new Random();
        int pivotIdx = rnd.nextInt(nTo-nFrom) + nFrom;

        Collections.swap(list, nFrom, pivotIdx);
        int value = list.get(nFrom);
        int high = nFrom+1;
        int low = nTo-1;

        do {
            while(high <= low &&  list.get(high).compareTo(value) <= 0)
                high++;
            while( list.get(low).compareTo(value) > 0 )
                low--;
            if(high < low)
                Collections.swap(list, high, low);
        }
        while(high < low);
        Collections.swap(list, low, nFrom);
        return low;
    }


}
