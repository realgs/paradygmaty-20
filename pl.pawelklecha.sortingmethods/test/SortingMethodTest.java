import org.junit.Assert;
import org.junit.Test;

import java.util.*;

public class SortingMethodTest {
    private final int ARRAY_SIZE = 500;

    private final Random random = new Random();

    private final SortingMethod<Integer> bubbleSortInteger = new BubbleSort<>();
    private final SortingMethod<Double> bubbleSortDouble = new BubbleSort<>();

    private final SortingMethod<Integer> insertionSortInteger = new InsertionSort<>();
    private final SortingMethod<Double> insertionSortDouble = new InsertionSort<>();

    @Test
    public void shouldSortWithBubbleSortEmptyList() {
        // given
        List<Integer> listOfPositiveIntegers = new ArrayList<>();
        List<Integer> listToSort = new ArrayList<>(listOfPositiveIntegers);

        // when
        bubbleSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveIntegers);

        // then
        Assert.assertEquals(listToSort, listOfPositiveIntegers);
    }

    @Test
    public void shouldSortWithBubbleSortPositiveIntegerValuesList() {
        // given
        List<Integer> listOfPositiveIntegers = generateRandomIntegerValues(false);
        List<Integer> listToSort = new ArrayList<>(listOfPositiveIntegers);

        // when
        bubbleSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveIntegers);

        // then
        Assert.assertEquals(listToSort, listOfPositiveIntegers);
    }

    @Test
    public void shouldSortWithBubbleSortMixedIntegerValuesList() {
        // given
        List<Integer> listOfPositiveIntegers = generateRandomIntegerValues(true);
        List<Integer> listToSort = new ArrayList<>(listOfPositiveIntegers);

        // when
        bubbleSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveIntegers);

        // then
        Assert.assertEquals(listToSort, listOfPositiveIntegers);
    }

    @Test
    public void shouldSortWithBubbleSortPositiveDoubleValuesList() {
        // given
        List<Double> listOfIntegers = generateRandomDoubleValues(false);
        List<Double> listToSort = new ArrayList<>(listOfIntegers);

        // when
        bubbleSortDouble.sort(listToSort);
        Collections.sort(listOfIntegers);

        // then
        Assert.assertEquals(listToSort, listOfIntegers);
    }

    @Test
    public void shouldSortWithBubbleSortMixedDoubleValuesList() {
        // given
        List<Double> listOfIntegers = generateRandomDoubleValues(true);
        List<Double> listToSort = new ArrayList<>(listOfIntegers);

        // when
        bubbleSortDouble.sort(listToSort);
        Collections.sort(listOfIntegers);

        // then
        Assert.assertEquals(listToSort, listOfIntegers);
    }

    @Test
    public void shouldSortWithInsertionSortEmptyList() {
        // given
        List<Integer> listOfPositiveIntegers = new ArrayList<>();
        List<Integer> listToSort = new ArrayList<>(listOfPositiveIntegers);

        // when
        bubbleSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveIntegers);

        // then
        Assert.assertEquals(listToSort, listOfPositiveIntegers);
    }

    @Test
    public void shouldSortWithInsertionSortPositiveIntegerValuesList() {
        // given
        List<Integer> listOfPositiveDoubles = generateRandomIntegerValues(false);
        List<Integer> listToSort = new ArrayList<>(listOfPositiveDoubles);

        // when
        insertionSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveDoubles);

        // then
        Assert.assertEquals(listToSort, listOfPositiveDoubles);
    }

    @Test
    public void shouldSortWithInsertionSortMixedIntegerValuesList() {
        // given
        List<Integer> listOfPositiveDoubles = generateRandomIntegerValues(true);
        List<Integer> listToSort = new ArrayList<>(listOfPositiveDoubles);

        // when
        insertionSortInteger.sort(listToSort);
        Collections.sort(listOfPositiveDoubles);

        // then
        Assert.assertEquals(listToSort, listOfPositiveDoubles);
    }


    @Test
    public void shouldSortWithInsertionSortPositiveDoubleValuesList() {
        // given
        List<Double> listOfPositiveDoubles = generateRandomDoubleValues(false);
        List<Double> listToSort = new ArrayList<>(listOfPositiveDoubles);

        // when
        insertionSortDouble.sort(listToSort);
        Collections.sort(listOfPositiveDoubles);

        // then
        Assert.assertEquals(listToSort, listOfPositiveDoubles);
    }

    @Test
    public void shouldSortWithInsertionSortMixedDoubleValuesList() {
        // given
        List<Double> listOfDoubles = generateRandomDoubleValues(true);
        List<Double> listToSort = new ArrayList<>(listOfDoubles);

        // when
        insertionSortDouble.sort(listToSort);
        Collections.sort(listOfDoubles);

        // then
        Assert.assertEquals(listToSort, listOfDoubles);
    }

    private List<Integer> generateRandomIntegerValues(Boolean generateWithNegativeValues) {
        List<Integer> randomNumbersList = new ArrayList<>();

        if (generateWithNegativeValues) {
            for (int i = 0; i < ARRAY_SIZE; i++)
                randomNumbersList.add(random.nextInt(100) * (random.nextBoolean() ? -1 : 1));
        } else {
            for (int i = 0; i < ARRAY_SIZE; i++)
                randomNumbersList.add(random.nextInt(100));
        }

        return randomNumbersList;
    }

    private List<Double> generateRandomDoubleValues(Boolean generateWithNegativeValues) {
        List<Double> randomNumbersList = new ArrayList<>();

        if (generateWithNegativeValues) {
            for (int i = 0; i < ARRAY_SIZE; i++)
                randomNumbersList.add(random.nextDouble() * (random.nextBoolean() ? -1 : 1));
        } else {
            for (int i = 0; i < ARRAY_SIZE; i++)
                randomNumbersList.add(random.nextDouble());
        }

        return randomNumbersList;
    }
}
