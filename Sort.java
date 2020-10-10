
public class Sort {
    public int[] bubbleSort(int table[])
    {
        int size = table.length;
        for (int i = 0; i< size - 1; i++)
        {
            for (int j = 0; j < size - i - 1; j++)
            {
                if (table[j] > table[j+1])
                {
                    int tmp = table[j];
                    table[j] = table[j+1];
                    table[j+1] = tmp;
                }
            }
        }
        return table;
    }
    int[] insertSort(int[] table)
    {
        int size = table.length;
        for (int i = 1; i < size; i++)
        {
            int key = table[i];
            int j = i - 1;
            for(; j >= 0 && table[j] > key; j--)
            {
                table[j+1] = table[j];
            }
            table[j+1] = key;
        }
        return table;
    }
}