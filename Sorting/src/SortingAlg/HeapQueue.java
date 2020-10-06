package SortingAlg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class HeapQueue<T> {
	private final List<T> list;
	private final Comparator<T> comparator;
	private  int swapCounter=0;
	public HeapQueue(Comparator<T> comp)
	{
		comparator=comp;
		list=new ArrayList<T>();
	}
	public void enqueue(T value) 
	{
		list.add(value);
		swim(list.size()-1);
	}
	public T dequeue() {
		if(list.size()>0) {
			T result=list.get(0);
			if(list.size()>1) 
			{
				list.set(0,list.get(list.size()-1));
				sink(0);
				list.remove(list.size()-1);
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
		list.set(index2,temp);
		swapCounter++;
	}
	
	protected void swim(int index) {
		int parent;
		while(index!=0&&comparator.compare(list.get(index),list.get(parent=(index-1)/2))>0) //jeœli element jest wiêkszy od rodzica to wyp³ywa
		{
			swap(index,parent);
			index=parent;
		}
	}

	protected void sink(int index) {
		boolean isDone = false;
		int child;
		while (!isDone && (child=2 * index + 1)< list.size()) {
			if (child < list.size() - 1 && comparator.compare(list.get(child), list.get(child + 1)) < 0)
				child++;
			if (comparator.compare(list.get(index), list.get(child)) < 0) {
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
