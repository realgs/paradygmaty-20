public class Main
{
    public static void main(String[] args)
    {
        
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
}
