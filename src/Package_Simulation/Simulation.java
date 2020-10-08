package Package_Simulation;

import java.util.Arrays;
import java.util.Random;
import Package_SortingMethods.*;

public class Simulation {

	public void simulation() {
		int[] sortTab = createTab();
		System.out.println("Input tab for bubble sort: ");
		System.out.println(Arrays.toString(sortTab));
		BubbleSort bubbleSort = new BubbleSort(sortTab);
		bubbleSort.sort();
		System.out.println("Output: ");
		System.out.println(Arrays.toString(sortTab));
		
		sortTab = createTab();
		System.out.println("Input tab for heap sort: ");
		System.out.println(Arrays.toString(sortTab));
		HeapSort heapSort = new HeapSort(sortTab);
		heapSort.sort();
		System.out.println("Output: ");
		System.out.println(Arrays.toString(sortTab));
	}
	
	private int[] createTab() {
		int[] numberTab = new int[25];
		Random generator = new Random();
		for (int i = 0; i < 25; i++) {
			numberTab[i] = generator.nextInt(100);
		}
		return numberTab;
	}
}
