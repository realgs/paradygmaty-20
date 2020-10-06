import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MergeSort<Double> {
    public ArrayList<Double> mergeSort(List<Double> list, int startIndex, int endIndex){
        if (startIndex == endIndex) {
            ArrayList<Double> result = new ArrayList<>();
            result.add(list.get(startIndex));
            return result;
        }
        int splitIndex = startIndex + (endIndex - startIndex) / 2;
        return merge(mergeSort(list, startIndex, splitIndex), mergeSort(list, splitIndex + 1, endIndex));
    }

    private ArrayList<Double> merge(List<Double> left, List<Double> right) {
        ArrayList<Double> result=new ArrayList<>();
        Iterator<Double> l = left.iterator();
        Iterator<Double> r = right.iterator();
        Double elemL=null;
        Double elemR=null;
        boolean contL;
        boolean contR;
        if(contL=l.hasNext()) elemL=l.next();
        if(contR=r.hasNext()) elemR=r.next();
        while (contL && contR) {
            if ((double)elemL<=(double)elemR){
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
