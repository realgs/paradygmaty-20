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

	public static void main(String[] args) {
				
		System.out.println(Arrays.toString(bubbleSort(new int [] {9,5,2,4,1})));
		
	}

}
