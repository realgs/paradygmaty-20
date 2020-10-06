package SortingAlorithms;

import java.util.ArrayList;
import java.lang.Number;

public class SelectSort {
    public <T extends Number & Comparable<? super T>> ArrayList<T> sort(ArrayList <T> list){
        for(int i=0; i<list.size(); i++){
            T minValue = list.get(i);
            int minValueIndex = i;
            for(int j=i+1; j<list.size(); j++){
                if(minValue.compareTo(list.get(j)) > 0) {
                    minValue = list.get(j);
                    minValueIndex = j;
                }
            }
            swap(list, i, minValueIndex);
        }
        return list;
    }

    private <T extends Number & Comparable<? super T>> void swap(ArrayList<T> list, int leftIdx, int rightIdx){
        T temp = list.get(leftIdx);
        list.set(leftIdx, list.get(rightIdx));
        list.set(rightIdx, temp);
    }
}
