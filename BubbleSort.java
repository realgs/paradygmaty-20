package SortingAlorithms;

import java.util.ArrayList;
import java.lang.Number;

public class BubbleSort {

    public <T extends Number & Comparable<? super T>> ArrayList<T> sort(ArrayList <T> list){
        for(int i=0; i<list.size()-1; i++){
            for(int j=0; j<list.size()-1-i; j++){
                if(list.get(j).compareTo(list.get(j+1)) > 0) {
                    swap(list, j, j+1);
                }
            }
        }
        return list;
    }

    private <T extends Number & Comparable<? super T>> void swap(ArrayList<T> list, int leftIdx, int rightIdx){
        T temp = list.get(leftIdx);
        list.set(leftIdx, list.get(rightIdx));
        list.set(rightIdx, temp);
    }
}
