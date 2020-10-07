package SortingAlg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.lang.Number;
import java.util.Random;

public class QuickSort<T extends Number> {
	private Random r = new Random();

	private ArrayList<T> toList(Iterable<T> iterable) {
		Iterator<T> it = iterable.iterator();
		ArrayList<T> list = new ArrayList<>();
		while (it.hasNext()) {
			list.add((T) it.next());
		}
		return list;
	}

	public List<T> sort(List<T> list) {
		quicksort(list, 0, list.size());
		return list;
	}

	public List<T> sort(Iterable<T> iterable) {
		List<T> list = this.toList(iterable);
		quicksort(list, 0, list.size());
		return list;
	}

	public List<T> sort(T[] array) {
		Iterable<T> iterable = Arrays.asList(array);
		List<T> list = this.toList(iterable);
		quicksort(list, 0, list.size());
		return list;
	}

	private void quicksort(List<T> list, int startIndex, int endIndex) {
		if (endIndex > startIndex + 1)
		{
			int punktPodzialu = divide(list, startIndex, endIndex);
			quicksort(list, startIndex, punktPodzialu);
			quicksort(list, punktPodzialu + 1, endIndex);
		}
	}

	private int divide(List<T> list, int leftb, int rightb) {
		int pivot = leftb + r.nextInt(rightb - leftb);
		swap(list, leftb, pivot);
		int indBigger = leftb + 1;
		int indLower = rightb - 1;
		T value = list.get(leftb);
		do {
			while (indLower >= indBigger && list.get(indBigger).doubleValue() <= value.doubleValue())
				indBigger++;
			while (list.get(indLower).doubleValue() > value.doubleValue())
				indLower--;
			if (indBigger < indLower) {
				swap(list, indBigger, indLower);
			}
		} while (indBigger < indLower);
		swap(list, leftb, indLower);
		return indLower;
	}

	private void swap(List<T> list, int left, int right) {
		if (left != right) {
			T temp = list.get(left);
			list.set(left, list.get(right));
			list.set(right, temp);
		}
	}
}
