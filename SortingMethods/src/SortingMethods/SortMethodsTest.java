package SortingMethods;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import SortingMethods.Shell_Sort;
import SortingMethods.NaturalComparator;
import SortingMethods.Counting_Sort;

class SortMethodsTest {

	Shell_Sort<Integer> SH_int;
	Counting_Sort CS;
	NaturalComparator<Integer> NaturalComparator;

	@BeforeEach
	void setUp() {
		NaturalComparator = new NaturalComparator<Integer>();
		SH_int = new Shell_Sort<Integer>(NaturalComparator);
		CS = new Counting_Sort();
	}

	@Test
	void Integers_8_Test() {
		int[] array1 = { 0, 4, 0, 2, 3, 3, 0, 5 };
		int[] array2;
		List<Integer> list1;
		List<Integer> list2;
		list1 = SH_int.toList(array1);
		list2 = SH_int.copy(list1);
		list2.sort(NaturalComparator);
		array2 = SH_int.toArray(list2);
		assertEquals(list2, SH_int.sort(list1));
		assertArrayEquals(array2, CS.sort(array1, 5));

	}

	@Test
	void HundredAlmostSortedTest() {
		int[] array1 = new int[100];
		int[] array2;
		List<Integer> list1 = new ArrayList<Integer>();
		List<Integer> list2;
		for (int i = 0; i < 100; i++) {
			int a = i;
			if (a % 10 == 0)
				a = a * 13 / 6;
			array1[i] = a;
			list1.add(a);
		}
		list2 = SH_int.copy(list1);
		list2.sort(NaturalComparator);
		array2 = SH_int.toArray(list2);
		assertEquals(list2, SH_int.sort(list1));
		assertArrayEquals(array2, CS.sort(array1, 200));
	}

	@Test
	void HundredReverseSortedTest() {
		int[] array1 = new int[100];
		int[] array2;
		List<Integer> list1 = new ArrayList<Integer>();
		List<Integer> list2;
		for (int i = 0; i < 100; i++) {
			int a = 100 - i;
			array1[i] = a;
			list1.add(a);
		}
		list2 = SH_int.copy(list1);
		list2.sort(NaturalComparator);
		array2 = SH_int.toArray(list2);
		assertEquals(list2, SH_int.sort(list1));
		assertArrayEquals(array2, CS.sort(array1, 100));
	}

	@Test
	void HundredRandomTest() {
		int[] array1 = new int[100];
		int[] array2;
		List<Integer> list1 = new ArrayList<Integer>();
		List<Integer> list2;
		Random r = new Random();
		for (int i = 0; i < 100; i++) {
			int a = r.nextInt(400);
			array1[i] = a;
			list1.add(a);
		}
		list2 = SH_int.copy(list1);
		list2.sort(NaturalComparator);
		array2 = SH_int.toArray(list2);
		assertEquals(list2, SH_int.sort(list1));
		assertArrayEquals(array2, CS.sort(array1, 400));
	}

}
