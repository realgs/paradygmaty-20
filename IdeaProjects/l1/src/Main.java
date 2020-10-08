public class Main {

    public static int partition(int[] array, int start, int end){
        return 0;
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

    public static void quickSort(int[] array){

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
