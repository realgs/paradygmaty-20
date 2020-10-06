import java.util.ArrayList;
import java.util.Random;

public class Main {
    public static void main(String[] args) {
        ArrayList<Integer> listQ = new ArrayList<>();
        ArrayList<Integer> listM = new ArrayList<>();
        Random rand = new Random();
        int number;
        int amount = rand.nextInt(30);
        for(int i=0; i<amount; i++){
            number=rand.nextInt(1000);
            listM.add(number);
            listQ.add(number);
        }
        QuickSort<Integer> qSort = new QuickSort<>();
        qSort.quickSort(listQ, 0, listQ.size()-1);
        for(int i=0; i<amount; i++){
            System.out.print(listQ.get(i)+" ");
        }

        System.out.println();

        MergeSort<Integer> mSort = new MergeSort<>();
        listM = mSort.mergeSort(listM, 0, listM.size()-1);
        for(int i=0; i<amount; i++){
            System.out.print(listM.get(i)+" ");
        }
    }
}
