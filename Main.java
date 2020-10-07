import java.util.ArrayList;

public class Main{
    public static void main(String[] args) {
        ArrayList<Integer> arrayList = new ArrayList<>();
        int min_num = -1000;
        int max_num = 1000;
        int array_len = 100;

        for (int i = 0; i < array_len; i++) {
            int r_num = (int) (Math.random()*((max_num - min_num)+1)) + min_num;
            arrayList.add(r_num);
        }

        BubbleSort bubbleSort = new BubbleSort(arrayList);
        for (int i = 0; i < bubbleSort.array.size(); i++) {
            System.out.print(bubbleSort.array.get(i) + " ");
        }
        System.out.println();

        int step = 2;
        ShellSort insertionSort = new ShellSort(arrayList,step);
        for (int i = 0; i < insertionSort.array.size(); i++) {
            System.out.print(insertionSort.array.get(i) + " ");
        }
    }
}
