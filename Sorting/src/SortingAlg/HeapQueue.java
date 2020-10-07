package SortingAlg;

import java.util.ArrayList;
import java.util.List;

public class HeapQueue<T extends Number> {
	private final List<T> list;
	private int swapCounter = 0;

	public HeapQueue() {
		list = new ArrayList<T>();
	}

	public void enqueue(T value) {
		list.add(value);
		swim(list.size() - 1);
	}

	public T dequeue() {
		if (list.size() > 0) {
			T result = list.get(0);
			if (list.size() > 1) {
				list.set(0, list.get(list.size() - 1));
				sink(0);
				list.remove(list.size() - 1);
			}
			return result;
		}
		return null;
	}

	public void clear() {
		list.clear();
	}

	protected void swap(int index1, int index2) {
		T temp = list.get(index1);
		list.set(index1, list.get(index2));
		list.set(index2, temp);
		swapCounter++;
	}

	protected void swim(int index) {
		int parent;
		while (index != 0 && list.get(index).doubleValue() < list.get(parent = (index - 1) / 2).doubleValue()) {
			swap(index, parent);
			index = parent;
		}
	}

	protected void sink(int index) {
		boolean isDone = false;
		int child;
		while (!isDone && (child = 2 * index + 1) < list.size()) {
			if (child < list.size() - 1 && list.get(child).doubleValue() > list.get(child + 1).doubleValue())
				child++;
			if (list.get(index).doubleValue() > list.get(child).doubleValue()) {
				swap(index, child);
				index = child;
			} else {
				isDone = true;
			}
		}
	}

	public int getSwapCounter() {
		return swapCounter;
	}

	public void setSwapCounter(int swapCounter) {
		this.swapCounter = swapCounter;
	}

}
