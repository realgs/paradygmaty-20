package Package_SortingMethods;

public class BubbleSort {
	
	private int[] ints;

	public BubbleSort(int[] ints) {
		this.ints = ints;
	}
	
	public void sort() {
		bubbleSort(this.ints);
	}
	
	private int[] bubbleSort(int[] ints) {
		int size = ints.length;
		for (int pass = 1; pass < size; ++pass) {
			for (int left = 0; left < (size - pass); ++left) {
				int right = left + 1;
				if (ints[left] > ints[right]) {
					swap(ints,left,right);
				}
			}
		}
		return ints;
	}
	
	private void swap(int[] ints, int left, int right) {
		int copyValue = ints[left];
		ints[left] = ints[right];
		ints[right] = copyValue;
	}
	
	public int[] getInts() {
		return ints;
	}

	public void setInts(int[] ints) {
		this.ints = ints;
	}
}
