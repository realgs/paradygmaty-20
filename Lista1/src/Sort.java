import java.util.Comparator;

public class Sort<T extends Number>
{
    private Comparator<T> comparator;

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

    public T[] quickSort(T[] arrayToSort, Comparator<T> comparator)
    {
        this.comparator = comparator;

        qSort(arrayToSort,0,arrayToSort.length - 1);

        return arrayToSort;
    }

    private void qSort(T[] arrayToSort, int startIndex, int endIndex)
    {
        if(startIndex < endIndex)
        {
            int splitIndex = split(arrayToSort,startIndex,endIndex);

            qSort(arrayToSort,startIndex,splitIndex - 1);
            qSort(arrayToSort,splitIndex + 1,endIndex);
        }
    }

    private int split(T[] arrayToSort, int startIndex, int endIndex)
    {
        int splitIndex = (startIndex + endIndex)/2;
        T splitObject = arrayToSort[splitIndex];
        int currentIndex = startIndex;
        swap(arrayToSort,splitIndex,endIndex);

        for(int i = startIndex; i < endIndex ; i++)
        {
            if(comparator.compare(arrayToSort[i],splitObject) < 0)
            {
                swap(arrayToSort,i,currentIndex++);
            }
        }

        swap(arrayToSort,currentIndex,endIndex);

        return currentIndex;
    }

    private void swap(T[] array, int firstIndex, int secondIndex)
    {
        T tmp = array[firstIndex];
        array[firstIndex] = array[secondIndex];
        array[secondIndex] = tmp;
    }
}
