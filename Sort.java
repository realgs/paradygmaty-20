import java.util.Random;

public class Sort{
    private int [] array;

    public Sort(int size){
        array = new int [size];
    }

    public int [] generateArray(int bound){
        Random r = new Random();
        for(int i = 0; i < array.length; i++){
            array[i] = r.nextInt(bound);
        }
        return array;
    }

    public void displayArray(){
        System.out.println("Array:");
        for(int i = 0; i < array.length; i++){
            System.out.print(array[i] + " ");
        }
        System.out.println(" ");
    }

    public void bubbleSort(int [] array){
        for (int i = 0; i < array.length; i++) {
            for (int j = i + 1; j < array.length; j++) {
                int temp = 0;
                if (array[i] > array[j]) {
                    temp = array[i];
                    array[i] = array[j];
                    array[j] = temp;
                }
            }
        }
    }

    public void selectionSort(int [] arr){ 
        for (int i = 0; i < arr.length - 1; i++) { 
            int min_index = i; 
            for (int j = i+1; j < arr.length; j++) 
                if (arr[j] < arr[min_index]) 
                    min_index = j; 
            int temp = arr[min_index]; 
            arr[min_index] = arr[i]; 
            arr[i] = temp; 
        } 
    } 
    
    public void removeDuplicate(int arr[]){
	    int j = 0;
	    int[] temp = new int[arr.length];

	    for (int i = 0; i < arr.length-1; i++) {
	        if (arr[i] != arr[i+1]){
	            temp[j] = arr[i];
	            j++;
	        }
	    }

	    temp[j++] = arr[arr.length-1];
	
	    for (int i = 0; i < j; i++){
	    	arr[i] = temp[i];
	    }
	    
	    for (int i = 0; i < j; i++){
	        System.out.print(arr[i]+" ");
        }
        System.out.println(" ");
    }
}