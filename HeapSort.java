package Algorithms;

public class HeapSort<T extends Number &  Comparable<? super T>> {
	
	public T[] sort(T array[]) {
		int n = array.length;
		
		for (int i = n / 2 - 1; i >= 0; i--) {
			heapify(array, i, n);
		}
		
		for(int i = n - 1; i >= 0; i--) {
			T temp = array[0];
			array[0] = array[i];
			array[i] = temp;
			
			heapify(array, 0, i);
		} 
		return array;
	}
	
	public void heapify(T[] array, int index, int n) {
		int max = index;
		int leftChild = 2 * index + 1;
		int rightChild = 2 * index + 2;
		
		if (leftChild < n && array[leftChild].compareTo(array[max]) > 0) {
			max = leftChild;
		}
		
		if (rightChild < n && array[rightChild].compareTo(array[max]) > 0) {
			max = rightChild;
		}
		
		if (max != index) {
			T swap = array[index];
			array[index] = array[max];
			array[max] = swap;
		
			heapify(array, max, n);
		}
	}
}
