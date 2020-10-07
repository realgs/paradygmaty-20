import java.util.Comparator;

public class Sort<T extends Number>
{
    public T[] insertionSort(T[] arrayToSort, Comparator<T> comparator)
    {
        for(int i = 1 ; i < arrayToSort.length ; i++)
        {
            for(int j = i ; j > 0 ; j--)
            {
                if(comparator.compare(arrayToSort[j],arrayToSort[j - 1]) < 0)
                {
                    swap(arrayToSort, j,j-1);
                }

                else break;
            }
        }

        return arrayToSort;
    }

    private void swap(T[] array, int firstIndex, int secondIndex)
    {
        T tmp = array[firstIndex];
        array[firstIndex] = array[secondIndex];
        array[secondIndex] = tmp;
    }
}