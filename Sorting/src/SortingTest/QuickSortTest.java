package SortingTest;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import SortingAlg.QuickSort;

class QuickSortTest {
	QuickSort<Integer> qs;

	@BeforeEach
	void setup() {
		qs = new QuickSort<Integer>();
	}

	@Test
	void Simpletest() {
		ArrayList<Integer> list = new ArrayList<Integer>();
		list.add(50);
		list.add(2);
		list.add(3);
		list.add(13);
		list.add(5);
		list.add(11);
		list.add(12);
		list.add(47);
		list.add(14);
		list.add(0);
		list.add(11);
		list.add(12);
		list.add(-1);
		ArrayList<Integer> sortedlist1 = new ArrayList<Integer>();
		sortedlist1.add(-1);
		sortedlist1.add(0);
		sortedlist1.add(2);
		sortedlist1.add(3);
		sortedlist1.add(5);
		sortedlist1.add(11);
		sortedlist1.add(11);
		sortedlist1.add(12);
		sortedlist1.add(12);
		sortedlist1.add(13);
		sortedlist1.add(14);
		sortedlist1.add(47);
		sortedlist1.add(50);
		assertEquals(sortedlist1, qs.sort(list));
	}

	@Test
	void reverseSorted() {
		ArrayList<Integer> rslist = new ArrayList<Integer>();
		ArrayList<Integer> slist = new ArrayList<Integer>();
		for (int i = 0; i < 100; i++) {
			slist.add(i);
			rslist.add(99 - i);
		}
		assertEquals(slist, qs.sort(rslist));
	}

	@Test
	void arrayQSSortTest() {
		ArrayList<Integer> sortedlist = new ArrayList<>();
		sortedlist.add(-1);
		sortedlist.add(2);
		sortedlist.add(3);
		sortedlist.add(5);
		sortedlist.add(11);
		sortedlist.add(12);
		sortedlist.add(13);
		sortedlist.add(47);
		sortedlist.add(50);
		Integer[] array = new Integer[] { 50, 2, 3, 13, 5, 11, 12, 47, -1 };
		assertEquals(sortedlist, qs.sort(array));
	}

}
