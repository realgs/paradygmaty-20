import java.util.Arrays;
import java.util.Random;
public class Main
{
    public static void main(String[] args)
    {
        if(bigRandomTest())
        {
            System.out.println("bigRandomTest passed");
        }
        else
        {
            System.out.println("bigRandomTest not passed");
        }

        if(bigReverseOrderedTest())
        {
            System.out.println("bigReverseOrderedTest passed");
        }
        else
        {
            System.out.println("bigReverseOrderedTest not passed");
        }

        if(zeroElementTest())
        {
            System.out.println("zeroElementTest passed");
        }
        else
        {
            System.out.println("zeroElementTest not passed");
        }

        if(bigOrderedTest())
        {
            System.out.println("bigOrderedTest passed");
        }
        else
        {
            System.out.println("bigOrderedTest not passed");
        }
    }
    
    public static void insertSort(int [] tab)
    {
        for(int i = 1;i<tab.length;i++)
        {
            int index_Act = i;

            while(index_Act > 0 && tab[index_Act] < tab[index_Act - 1])
            {
                swap(tab,index_Act,index_Act - 1);
                index_Act--;
            }
        }
    }

    public static void bubbleSort(int [] tab)
    {
        for(int i = 0;i<tab.length;i++)
        {
            boolean isSorted = true;

            for(int j = 0;j<tab.length - i - 1;j++)
            {
                if(tab[j] > tab[j + 1])
                {
                    swap(tab,j,j + 1);
                    isSorted = false;
                }
            }

            if(isSorted)
            {
                break;
            }
        }
    }

    public static void swap(int [] tab,int firstIndex,int secondIndex)
    {
        int tmp = tab[firstIndex];
        tab[firstIndex] = tab[secondIndex];
        tab[secondIndex] = tmp;
    }

    public static void displayArray(int [] tab)
    {
        for(int i = 0;i<tab.length;i++)
        {
            System.out.print(tab[i] + " ");
        }

        System.out.println();
    }

    public static boolean bigRandomTest()
    {
        int [] bubbleArray = new int[30000];
        int [] insertArray = new int[30000];
        int [] bulitinArray = new int[30000];

        Random random = new Random();

        for(int i = 0;i<30000;i++)
        {
            int element = random.nextInt(10000000);

            bubbleArray[i] = element;
            insertArray[i] = element;
            bulitinArray[i] = element;
        }

        insertSort(insertArray);
        bubbleSort(bubbleArray);
        Arrays.sort(bulitinArray);

        return equalArrays(insertArray,bubbleArray) && equalArrays(bubbleArray,bulitinArray);
    }

    public static boolean bigReverseOrderedTest()
    {
        int [] bubbleArray = new int[30000];
        int [] insertArray = new int[30000];
        int [] bulitinArray = new int[30000];

        for(int i = 0;i<30000;i++)
        {
            int element = 30000 - i;

            bubbleArray[i] = element;
            insertArray[i] = element;
            bulitinArray[i] = element;
        }

        insertSort(insertArray);
        bubbleSort(bubbleArray);
        Arrays.sort(bulitinArray);

        return equalArrays(insertArray,bubbleArray) && equalArrays(bubbleArray,bulitinArray);
    }

    public static boolean zeroElementTest()
    {
        int [] bubbleArray = new int[0];
        int [] insertArray = new int[0];
        int [] bulitinArray = new int[0];

        insertSort(insertArray);
        bubbleSort(bubbleArray);
        Arrays.sort(bulitinArray);

        return equalArrays(insertArray,bubbleArray) && equalArrays(bubbleArray,bulitinArray);
    }

    public static boolean bigOrderedTest()
    {
        int [] bubbleArray = new int[30000];
        int [] insertArray = new int[30000];
        int [] bulitinArray = new int[30000];

        for(int i = 0;i<30000;i++)
        {
            int element = i;

            bubbleArray[i] = element;
            insertArray[i] = element;
            bulitinArray[i] = element;
        }

        insertSort(insertArray);
        bubbleSort(bubbleArray);
        Arrays.sort(bulitinArray);

        return equalArrays(insertArray,bubbleArray) && equalArrays(bubbleArray,bulitinArray);
    }

    public static boolean equalArrays(int [] firstArray,int [] secondArray)
    {
        for(int i = 0;i<firstArray.length;i++)
        {
            if(firstArray[i] != secondArray[i])
            {
                return false;
            }
        }
        return true;
    }
}
