import java.util.List;

public class SortingAlgorithms {
    public Integer[] insertSort(Integer[] unsortedArray) {
        for (int i = 1; i < unsortedArray.length; i++) {
            int selectedElement = unsortedArray[i];
            int j = i - 1;
            while (j >= 0 && unsortedArray[j] > selectedElement) {
                unsortedArray[j + 1] = unsortedArray[j];
                j--;
            }
            unsortedArray[j + 1] = selectedElement;
        }
        return unsortedArray;
    }

    public List<Integer> quickSort(List<Integer> list) {
        quickSort(list, 0, list.size() - 1);
        return list;
    }

    private void quickSort(List<Integer> list, int startIndex, int endIndex) {
        if (endIndex > startIndex) {
            int partition = partition(list, startIndex, endIndex);
            quickSort(list, startIndex, partition);
            quickSort(list, partition + 1, endIndex);
        }
    }

    private int partition(List<Integer> list, int left, int right) {
        int value = list.get(right);
        int i = left - 1;
        while (left <= right) {
            if (value >= list.get(left))
                swap(list, ++i, left);
            ++left;
        }
        return i < right ? i : i - 1;
    }

    private void swap(List<Integer> list, int left, int right) {
        if (left != right) {
            int temp = list.get(left);
            list.set(left, list.get(right));
            list.set(right, temp);
        }
    }
}
