import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Random;

public class Main {
    public static void main(String[] args) {
        ArrayList<Double> listQ = new ArrayList<>();
        ArrayList<Double> listM = new ArrayList<>();
        Random rand = new Random();
        double number;
        int amount = rand.nextInt(40);
        for(int i=0; i<amount; i++){
            number=(rand.nextFloat()-0.3)*100000;
            listM.add(number);
            listQ.add(number);
        }
        QuickSort<Double> qSort = new QuickSort<>();
        qSort.quickSort(listQ, 0, listQ.size()-1);
        for(int i=0; i<amount; i++){
            System.out.print(listQ.get(i)+" ");
        }

        System.out.println();

        MergeSort<Double> mSort = new MergeSort<>();
        listM = mSort.mergeSort(listM, 0, listM.size()-1);
        for(int i=0; i<amount; i++){
            System.out.print(listM.get(i)+" ");
        }
    }
}
