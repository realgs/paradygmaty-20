import java.util.ArrayList;

public class BubbleSort {
    ArrayList<Integer> array;

    public BubbleSort(ArrayList<Integer> arrayList) {
        System.out.println("Buble sort:");
        array = new ArrayList<>();
        int arrayList_size = arrayList.size();
        for (int i = 0; i < arrayList_size; i++) {
            array.add(arrayList.get(i));
        }
        bubbleSort(array);
    }

    public void bubbleSort(ArrayList<Integer> array) {
        int array_size = array.size();
        for (int i = 0; i < array_size; i++) {
            int curent_num = 0;
            int next_num = 0;
            int sense_counter = 0;
            for (int j = 0; j < array_size - 1 - i; j++) {
                curent_num = array.get(j);
                next_num = array.get(j + 1);
                if (curent_num > next_num) {
                    array.set(j, next_num);
                    array.set(j + 1, curent_num);
                    sense_counter ++;
                }
            }
            if (sense_counter == 0){
                break;
            }
        }
    }
}
