package Algorithms;

import java.util.Random;
import java.util.Scanner;

public class IntegerArray {
	
	public static void main(String[] args) {
	
		HeapSort heapsort = new HeapSort();
		MergeSort mergesort = new MergeSort();
	
		Scanner sc = new Scanner(System.in);
		System.out.println("Enter the size of array of Integer to sort: ");
		int size = sc.nextInt();
		Integer[] array1 = new Integer[size];
		Integer[] array2 = new Integer[size];
	
		Random rand = new Random();
	
		for (int i = 0; i < size; i++) {
			Integer number = rand.nextInt(1000);
			array1[i] = number;
			array2[i] = number;
		}
		heapsort.sort(array1);
		System.out.println("Sorting with heapsort algorithm: ");
		printArray(array1);
		mergesort.sort(array2, 0, array2.length - 1);
		System.out.println("Sorting with mergesort algorithm: ");
		printArray(array2);
		
	}
	
	public static void printArray(Integer[] array) {
		for (int i = 0; i < array.length - 1; i++) {
			System.out.print(array[i] + " ");
		}
		System.out.println();
	}
}
