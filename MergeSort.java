package Algorithms;

public class MergeSort<T extends Number & Comparable<? super T>> {
	
	public T[] sort(T[] array, int left, int right) {
		if (left < right) {
			int mid = (left + right) / 2;
			sort(array, left, mid);
			sort(array, mid + 1, right);
			merge(array, left, mid, right);
		}
		return array;
	}
	
	public void merge(T[] array, int left, int mid, int right) {
		int left1 = left, right1 = mid;
		int left2 = mid + 1, right2 = right;
		
		T[] array2 = (T[]) new Number[array.length];
		int i = left1;
		
		while((left1 <= right1) && (left2 <= right2)) {
			if (array[left1].compareTo(array[left2]) <= 0) {
				array2[i] = array[left1];
				left1++;
			} else {
				array2[i] = array[left2];
				left2++;
			}
			i++;
		}
		while (left1 <= right1) {
			array2[i] = array[left1];
			left1++;
			i++;
		}
		while(left2 <= right2) {
			array2[i] = array[left2];
			left2++;
			i++;
		}
		for (i = left; i <= right; i++) {
			array[i] = array2[i];
		}
	}

}
