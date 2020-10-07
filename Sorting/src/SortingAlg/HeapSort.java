package SortingAlg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class HeapSort<T extends Number> {
	HeapQueue<T> heap;

	public HeapSort() {
		heap = new HeapQueue<T>();
	}

	public List<T> sort(List<T> unsortedlist) {
		heap.clear();
		int bound = unsortedlist.size();
		for (int i = 0; i < bound; i++) {
			heap.enqueue(unsortedlist.get(i));
		}
		unsortedlist.clear();
		for (int i = 0; i < bound; i++) {
			unsortedlist.add(heap.dequeue());
		}
		return unsortedlist;
	}

	public List<T> sort(T[] array) {
		Iterable<T> iterable = Arrays.asList(array);
		return sort(iterable);
	}

	public List<T> sort(Iterable<T> iterable) {
		ArrayList<T> ilist = new ArrayList<>();
		Iterator<T> it = iterable.iterator();
		while (it.hasNext()) {
			ilist.add(it.next());
		}
		return sort(ilist);
	}

	public HeapQueue<T> getHeap() {
		return heap;
	}

}
