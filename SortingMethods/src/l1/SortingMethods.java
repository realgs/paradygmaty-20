package l1;

import java.util.Random;

public class SortingMethods {
	
	private static void swap(int array[], int left, int right) {
		int temp = array[left];
		array[left] = array[right];
		array[right] = temp;
	}
	
	public static void selectSort(int array[]) {
		int size = array.length;
		
		for(int i = 0; i < size-1; i++) {
			int idxMin = i;
			for(int j = i+1; j < size; j++) {
				if(array[j] < array[idxMin]) {
					idxMin = j;
				}
			}
			swap(array, idxMin, i);
		}
	}
	
	private static int partition(int array[], int idxBot, int idxTop) {
        int pivot = array[idxTop];
        //indeks mniejszego elementu
        int i = (idxBot-1);
        for (int j=idxBot; j<idxTop; j++) {
            if (array[j] < pivot) {
                i++;
                swap(array, i, j);
            }
        }
        swap(array, i+1, idxTop);
        return i+1;
    }

    private static void quickSortRec(int array[], int idxBot, int idxTop) {
        if (idxBot < idxTop) {
            int pivot = partition(array, idxBot, idxTop);
            quickSortRec(array, idxBot, pivot-1);
            quickSortRec(array, pivot+1, idxTop);
        }
    }
    
    public static void quickSort(int array[]) {
    	quickSortRec(array, 0, array.length-1);
    }
	
	public static void printArray(int array[]) {
		for(int i = 0; i < array.length; i++) {
			System.out.print(array[i]+", ");
		}
		System.out.println();
	}
	
	public static void main(String[] args) {
		int[] array1 = {8,2,4,7,4,5,9};
		int[] array2 = {4,10,7,23,5,-2,11,-4,0};
		int[] array3 = new int[50];
		Random rand = new Random();
		for(int i = 0; i < array3.length; i++) {
			array3[i] = rand.nextInt(100)-50;
		}
		
		selectSort(array1);
		quickSort(array2);
		quickSort(array3);
		
		printArray(array1);
		printArray(array2);
		printArray(array3);
	}


}
