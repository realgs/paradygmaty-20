package SortingMethods;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class Shell_Sort<T extends Number & Comparable<? super T>> {

	private final Comparator<T> _comparator;

	public Shell_Sort(Comparator<T> comparator) {
		_comparator = comparator;
	}

	public List<T> sort(List<T> list) {
		k_sort(list, list.size() / 2);
		return list;
	}

	private List<T> k_sort(List<T> list, int k) {
		int i, j;
		@SuppressWarnings("unchecked")
		ArrayList<Integer>[] indexs = new ArrayList[k];
		for (i = 0; i < indexs.length; i++) {
			indexs[i] = new ArrayList<Integer>();
		}

		for (j = 0; j < list.size(); j++) {
			int index = j % k;
			indexs[index].add(j);
		}
		for (i = 0; i < indexs.length; i++) {
			for (j = 1; j < indexs[i].size(); j++) {
				T value = list.get(indexs[i].get(j)), temp;
				int h;
				for (h = j; h > 0 && _comparator.compare(value, temp = list.get(indexs[i].get(h - 1))) < 0; --h) {
					list.set(indexs[i].get(h), temp);
				}
				list.set(indexs[i].get(h), value);
			}
		}
		if (k > 1)
			k_sort(list, k / 2);
		return list;
	}

	public List<T> copy(List<T> list) {
		List<T> list2 = new ArrayList<T>();
		list2.addAll(list);
		return list2;
	}

	public List<Integer> toList(int[] table) {
		List<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < table.length; i++) {
			list.add(table[i]);
		}
		return list;
	}

	public int[] toArray(List<Integer> list) {
		int[] array2 = new int[list.size()];
		for (int i = 0; i < list.size(); i++) {
			array2[i] = list.get(i);
		}
		return array2;
	}

}
