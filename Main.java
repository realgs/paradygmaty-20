
class Main{
    public static void main(String[] args) {
        Sort sort = new Sort();
        int table[] = {4,5,1,3,2,6};

        int[] sorted = sort.bubbleSort(table);
        for (int i = 0; i < sorted.length; i++)
        {
            System.out.print(sorted[i] + " ");
        }
        System.out.println();
        
        sorted = sort.insertSort(table);
        for (int i = 0; i < sorted.length; i++)
        {
            System.out.print(sorted[i] + " ");
        }
    }
}
