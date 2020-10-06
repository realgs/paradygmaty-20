import java.util.ArrayList;
import java.util.Random;

public class QuickSort {
    public void sort(ArrayList<Integer> list){
        quickSort(list,0,list.size());
    }
    private void quickSort(ArrayList<Integer> list, int startIndex, int endIndex) {
        int size = endIndex - startIndex;
        if (size > 1) {
            boolean found = false;
            Random rand = new Random();
            int index = rand.nextInt(size)+startIndex;
            int pev = list.get(index);
            swap(list, index, startIndex);
            int idxLower = endIndex-1;
            int idxBigger = startIndex+1;
            int same = 0;
            do{
                while(idxBigger<=idxLower && list.get(idxBigger).compareTo(pev)<=0) {
                    if(list.get(idxBigger).compareTo(pev)==0) {
                        same++;
                        if(!found){
                            swap(list, idxBigger, startIndex);
                            found = true;
                            idxBigger--;
                            same--;
                        }
                    }
                    idxBigger++;
                }
                while(list.get(idxLower).compareTo(pev)>0)
                    idxLower--;
                if(idxBigger<idxLower)
                    swap(list,idxBigger,idxLower);
            }while(idxBigger<idxLower);
            swap(list, idxLower, startIndex);
            if(same+1!=size) {
                quickSort(list, startIndex, idxLower);
                quickSort(list, idxLower + 1, endIndex);
            }
        }
    }
    private void swap(ArrayList<Integer> list, int left, int right) {
        if (left != right) {
            int tmp = list.get(left);
            list.set(left, list.get(right));
            list.set(right, tmp);
        }
    }
}

