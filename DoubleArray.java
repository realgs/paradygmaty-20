package Algorithms;

import java.util.Random;
import java.util.Scanner;

public class DoubleArray {
	
	public static void main(String[] args) {
		HeapSort heapsort = new HeapSort();
		MergeSort mergesort = new MergeSort();
		
		Scanner sc = new Scanner(System.in);
		System.out.println("Enter the size of array of Double to sort: ");
		int size = sc.nextInt();
		Double[] array3 = new Double[size];
		Double[] array4 = new Double[size];
		
		Random rand = new Random();
	
		for (int i = 0; i < size; i++) {
			Double number = rand.nextDouble() * 1000;
			array3[i] = number;
			array4[i] = number;
		}
		heapsort.sort(array3);
		System.out.println("Sorting with heapsort algorithm: ");
		printArray(array3);
		mergesort.sort(array4, 0, array4.length - 1);
		System.out.println("Sorting with mergesort algorithm: ");
		printArray(array4);
	}

	public static void printArray(Double[] array) {
		for (int i = 0; i < array.length - 1; i++) {
			System.out.printf(" %.2f",array[i]);
		}
		System.out.println();
	}
}
