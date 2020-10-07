import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.ArrayList;
import static org.junit.Assert.assertEquals;

public class SortTests {

    private Sort sorter = new Sort();
    ArrayList<Integer> expectedList;
    ArrayList<Integer> randomList;

    @Before
    public void setUp(){
        expectedList = new ArrayList<>();
        expectedList.add(1);
        expectedList.add(2);
        expectedList.add(5);
        expectedList.add(6);
        expectedList.add(17);
        expectedList.add(18);
        expectedList.add(44);

        randomList = new ArrayList<>();
        randomList.add(44);
        randomList.add(5);
        randomList.add(2);
        randomList.add(18);
        randomList.add(17);
        randomList.add(1);
        randomList.add(6);
    }

    @Test
    public void testBubbleSort(){
        assertEquals(expectedList, sorter.bubbleSort(randomList));
    }

    @Test
    public void testQuickSort(){
        assertEquals(expectedList, sorter.quickSort(randomList));
    }

    @Test
    public void testOneItemListBubbleS(){
        ArrayList<Integer> smallList = new ArrayList<>();
        smallList.add(2);
        ArrayList<Integer> sList = new ArrayList<>();
        sList.add(2);

        assertEquals(sList, sorter.bubbleSort(smallList));
    }

    @Test
    public void testOneItemListQuickS(){
        ArrayList<Integer> smallList = new ArrayList<>();
        smallList.add(2);
        ArrayList<Integer> sList = new ArrayList<>();
        sList.add(2);

        assertEquals(sList, sorter.quickSort(smallList));
    }

    @Test
    public void testEmptyList(){
        ArrayList<Integer> emptyList = new ArrayList<>();
        assertEquals(emptyList, sorter.bubbleSort(emptyList));
        assertEquals(emptyList, sorter.quickSort(emptyList));
    }

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testNullListBubbleS(){
        ArrayList<Integer> nullList = null;
        thrown.expect(IllegalArgumentException.class);
        sorter.bubbleSort(nullList);
    }

    @Test
    public void testNullListQuickS(){
        ArrayList<Integer> nullList = null;
        thrown.expect(IllegalArgumentException.class);
        sorter.quickSort(nullList);
    }

}
