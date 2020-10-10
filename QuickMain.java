package QuickSort;

import java.awt.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

public class QuickMain {

	public static void main(String[] args) {

		ArrayList<Integer> array = new ArrayList<Integer>();
		array.add(-34);
		array.add(1);
		array.add(24);
		array.add(53);
		array.add(75);
		array.add(98);
		array.add(712);
		array.add(712);
		array.add(876);
		array.add(986);

		Collections.shuffle(array);

		for (Integer numbers : array) {
			System.out.print(numbers + " ");
		}

		System.out.println();

		bubbleSort(array);

		for (Integer numbers : array) {
			System.out.print(numbers + " ");
		}
		
		System.out.println();

		Collections.shuffle(array);

		quickSort(array, 0, array.size() - 1);

		for (Integer numbers : array) {
			System.out.print(numbers + " ");
		}

	}

	public static void quickSort(ArrayList<Integer> list, int firstIndex, int lastIndex) {

		if (firstIndex < lastIndex) {
			int separateIndex = separated(list, firstIndex, lastIndex);
			quickSort(list, firstIndex, separateIndex - 1);
			quickSort(list, separateIndex, lastIndex);
		}

	}

	public static int separated(ArrayList list, int from, int to) {
		int rIndex = to;
		int lIndex = from;
		int separatIndex = (int) (Math.random() * (from - to + 1) + (to - 1));
		int pivot = (int) list.get(separatIndex);

		while (lIndex <= rIndex) {

			while ((int) list.get(lIndex) < pivot) {
				lIndex++;
			}

			while ((int) list.get(rIndex) > pivot) {
				rIndex--;
			}

			if (lIndex <= rIndex) {
				swap(list, rIndex, lIndex);
				lIndex++;
				rIndex--;
			}

		}

		return lIndex;
	}

	public static void swap(ArrayList<Integer> list, int index1, int index2) {
		int temp = list.get(index1);
		list.set(index1, list.get(index2));
		list.set(index2, temp);
	}

	public static void bubbleSort(ArrayList<Integer> list) {
		int index1 = 0;
		int index2 = 1;

		for (int i = 0; i < list.size(); i++) {
			while (index1 < (list.size() - 1)) {

				if (list.get(index1) > list.get(index2)) {
					int temp = list.get(index1);
					list.set(index1, list.get(index2));
					list.set(index2, temp);

				}
				index1++;
				index2++;
			}
			index1 = 0;
			index2 = 1;
		}

	}
}
