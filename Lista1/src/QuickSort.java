import java.util.List;

public class QuickSort<Integer> {
    public void quickSort(List<Integer> list, int begin, int end){
        if(begin<end){
            int pivotIndex = partition(list, begin, end);
            quickSort(list, begin, pivotIndex-1);
            quickSort(list, pivotIndex+1, end);
        }
    }

    private int partition(List<Integer> list, int begin, int end){
        int pivot = (int)list.get(end);
        int i = (begin-1);
        Integer temp;

        for (int j = begin; j < end; j++) {
            if((int)list.get(j)<=pivot){
                i++;
                temp = list.get(i);
                list.set(i, list.get(j));
                list.set(j, temp);
            }
        }

        temp = list.get(i+1);
        list.set(i+1, list.get(end));
        list.set(end, temp);
        return i+1;
    }

}
