public class Main
{
    public static void main(String[] args)
    {

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
