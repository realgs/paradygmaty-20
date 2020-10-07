package SortingMethods;

public class Counting_Sort {

	public int[] sort(int arr[], int k) {
		k++;
		int n = arr.length;
		int[] pos = new int[k];
		int[] result = new int[n];
		int i, j;

		for (j = 0; j < n; j++) {
			pos[arr[j]]++;
		}
		pos[0]--;

		for (i = 1; i < k; i++)
			pos[i] += pos[i - 1];

		for (j = n - 1; j >= 0; j--) {
			result[pos[arr[j]]] = arr[j];
			pos[arr[j]]--;
		}

		for (j = 0; j < n; j++)
			arr[j] = result[j];

		return arr;
	}
}
