package Package_SortingMethods;

public class HeapSort {
	
	private int[] ints;
	
	public HeapSort(int[] ints) {
		this.ints = ints;
	}
	
	public void sort() {
		heapSort(this.ints, this.ints.length);
	}
	
	private int[] heapSort(int[] ints, int n) {
		adjustHeap(ints,n);
		for (int i = n - 1; i > 0; i--) {
			swap(ints,i,0);
			heapify(ints,0,i);
		}
		return ints;
	}
	
	private void swap(int[] ints, int left, int right) {
		int copyValue = ints[left];
		ints[left] = ints[right];
		ints[right] = copyValue;
	}
	
	public void heapify(int[] ints, int index, int n) {
		int indexBigger = 2*index + 1;
		if (indexBigger < n) {
			if ( (indexBigger + 1) < n && ints[indexBigger] < ints[indexBigger + 1]) {
				indexBigger++;
			}
			if (ints[index] < ints[indexBigger]) {
				swap(ints,index,indexBigger);
				heapify(ints,indexBigger,n);
			}
		}
	}
	
	public void adjustHeap(int[] ints, int n) {
		for (int i = (n - 1)/2; i >= 0; i--) {
			heapify(ints, i, n);
		}
	}

	public int[] getInts() {
		return ints;
	}

	public void setInts(int[] ints) {
		this.ints = ints;
	}
}
