import java.util.ArrayList;

public class SelectSort
{
    public <T extends Comparable<? super T>> void sort(ArrayList<T> lista)
    {
        int size = lista.size();
        for (int i = 0 ; i < (size - 1) ; ++i)
        {
            int smallest = i;
            for(int j = i + 1; j  < size ; ++j)
            {
                if(lista.get(j).compareTo(lista.get(smallest)) < 0)
                {
                    smallest = j;
                }
            }
            if(smallest != i)
                swap(lista, smallest, i);
        }
    }
    private static <T> void swap(ArrayList<T> list, int left, int right)
    {
        T tmp = list.get(left);
        list.set(left, list.get(right));
        list.set(right, tmp);
    }
}
