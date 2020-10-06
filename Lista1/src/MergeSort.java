import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MergeSort<Integer> {
    public ArrayList<Integer> mergeSort(List<Integer> list, int startIndex, int endIndex){
        if (startIndex == endIndex) {
            ArrayList<Integer> result = new ArrayList<>();
            result.add(list.get(startIndex));
            return result;
        }
        int splitIndex = startIndex + (endIndex - startIndex) / 2;
        return merge(mergeSort(list, startIndex, splitIndex), mergeSort(list, splitIndex + 1, endIndex));
    }

    private ArrayList<Integer> merge(List<Integer> left, List<Integer> right) {
        ArrayList<Integer> result=new ArrayList<>();
        Iterator<Integer> l = left.iterator();
        Iterator<Integer> r = right.iterator();
        Integer elemL=null;
        Integer elemR=null;
        boolean contL;
        boolean contR;
        if(contL=l.hasNext()) elemL=l.next();
        if(contR=r.hasNext()) elemR=r.next();
        while (contL && contR) {
            if ((int)elemL<=(int)elemR){
                result.add(elemL);
                if(contL=l.hasNext()) elemL=l.next();
                else result.add(elemR);
            }else {
                result.add(elemR);
                if(contR=r.hasNext()) elemR=r.next();
                else result.add(elemL);
            }
        }
        while(l.hasNext()){
            result.add(l.next());
        }
        while(r.hasNext()){
            result.add(r.next());
        }
        return result;
    }
}
