public class Main {

    public static int partition(int[] array, int start, int stop){
        int pivot = array[stop];
        int i = (start-1);

        for (int j = start; j < stop; j++) {
            if (array[j] <= pivot) {
                i++;

                int swapTemp = array[i];
                array[i] = array[j];
                array[j] = swapTemp;
            }
        }

        int swapTemp = array[i+1];
        array[i+1] = array[stop];
        array[stop] = swapTemp;

        return i+1;
    }

    public static void shellSort(int arrayToSort[]) {
        int n = arrayToSort.length;

        for (int gap = n / 2; gap > 0; gap /= 2) {
            for (int i = gap; i < n; i++) {
                int key = arrayToSort[i];
                int j = i;
                while (j >= gap && arrayToSort[j - gap] > key) {
                    arrayToSort[j] = arrayToSort[j - gap];
                    j -= gap;
                }
                arrayToSort[j] = key;
            }
        }
    }

    public static void quickSort(int[] array, int start, int stop){
        if(start<stop){
            int index=partition(array, start, stop);
            quickSort(array, start, index-1);
            quickSort(array, index+1, stop);
        }
    }

    public static void main(String[] args){
        int[] arr=new int[10];
        for(int i=10; i>0; i--){
            arr[10-i]=i;
        }
        shellSort(arr);
        for(int i=0; i<10; i++){
            System.out.println(arr[i]);
        }
    }
}
