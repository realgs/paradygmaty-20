import java.util.ArrayList;

public class ShellSort {
    ArrayList<Integer> array;

    public ShellSort(ArrayList<Integer> arrayList, int step) {
        System.out.println("Shell sort:");
        array = new ArrayList<>();
        int arrayList_size = arrayList.size();
        for (int i = 0; i < arrayList_size; i++) {
            array.add(arrayList.get(i));
        }
        shellSort(array, step);
    }

    public void shellSort(ArrayList<Integer> array, int step) {
        int array_size = array.size();
        for (int interval = array_size / step; interval > 0; interval /= step) {
            for (int i = interval; i < array_size; i++) {
                for (int j = i - interval; j >= 0; j = j - interval) {
                    if (array.get(j + interval) > array.get(j)) {
                        break;
                    }
                    else {
                        int num = array.get(j);
                        array.set(j,array.get(j+interval));
                        array.set(j+interval,num);
                    }
                }
            }
        }
    }
}