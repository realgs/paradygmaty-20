
// Autor: Pawel Winiarz, 218717
// Paradygmaty Programowania Lab, Wt. godz. 19

import java.util.Arrays;

public class main {

	static int[] bubbleSort(int[] tab) {
		if (tab.length > 1) {
			int tmp;
			boolean isSorted = false;
			while (!isSorted) {
				isSorted = true;
				for (int i = 0; i < tab.length - 1; i++) {
					if (tab[i] > tab[i + 1]) {
						isSorted = false;
						tmp = tab[i];
						tab[i] = tab[i + 1];
						tab[i + 1] = tmp;
					}
				}
			}
		}
		return tab;
	}

	static int[] selectSort(int[] tab) {
		if (tab.length > 1) {
			int minVal = Integer.MAX_VALUE;
			int whereMinVal = -1;
			int tmp;
			boolean isSorted = false;
			for (int i = 0; i < tab.length && !isSorted; i++) {
				isSorted = true;
				for (int j = i; j < tab.length; j++) {
					if (tab[j] < minVal) {
						minVal = tab[j];
						whereMinVal = j;
						isSorted = false;
					}
				}
				if (!isSorted) {
					tmp = tab[i];
					tab[i] = minVal;
					tab[whereMinVal] = tmp;

					whereMinVal = -1;
					minVal = Integer.MAX_VALUE;
				}
			}
		}
		return tab;
	}

	public static void main(String[] args) {

		System.out.println(Arrays.toString(bubbleSort(new int[] { 9, 5, 2, 4, 1 })));
		System.out.println(Arrays.toString(selectSort(new int[] { 9, 5, 2, 4, 1 })));

	}

}
