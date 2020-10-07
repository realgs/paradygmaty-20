import java.util.Comparator;

public class Sort<T extends Number>
{
    private T[] arrayToSort;

    public T[] insertionSort(T[] arrayToSort, Comparator<T> comparator)
    {
        this.arrayToSort = arrayToSort;

        for(int i = 1 ; i < arrayToSort.length ; i++)
        {
            for(int j = i ; j > 0 ; j--)
            {
                if(comparator.compare(arrayToSort[j],arrayToSort[j - 1]) < 0)
                {
                    swap(j,j-1);
                }

                else break;
            }
        }

        return arrayToSort;
    }

    private void swap(int firstIndex, int secondIndex)
    {
        T tmp = arrayToSort[firstIndex];
        arrayToSort[firstIndex] = arrayToSort[secondIndex];
        arrayToSort[secondIndex] = tmp;
    }
}
