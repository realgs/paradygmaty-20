import java.util.ArrayList;

public class main {
    public static void main(String[] args)
    {
        ArrayList<Integer> intList100 = ValueGenerator.generateIntegers(100);
        ArrayList<Integer> intList1000 = ValueGenerator.generateIntegers(1000);
        ArrayList<Integer> intList10000 = ValueGenerator.generateIntegers(10000);

        ArrayList<Integer> intList9 = ValueGenerator.generateSpecificIntegerList(1000, 9);
        ArrayList<Integer> intList0 = ValueGenerator.generateSpecificIntegerList(1000, 0);
        ArrayList<Integer> intList3 = ValueGenerator.generateSpecificIntegerList(1000, 3);

        ArrayList<Integer> r_intList100 = ValueGenerator.reverseTheList(intList100);
        ArrayList<Integer> r_intList1000 = ValueGenerator.reverseTheList(intList1000);
        ArrayList<Integer> r_intList10000 = ValueGenerator.reverseTheList(intList10000);

        QuickSort quickSort = new QuickSort();
        quickSort.sort(intList100);     //random
        quickSort.sort(intList1000);
        quickSort.sort(intList10000);
        quickSort.sort(intList9);       //one-digit
        quickSort.sort(intList0);
        quickSort.sort(intList3);
        quickSort.sort(r_intList100);   //reversed random
        quickSort.sort(r_intList1000);
        quickSort.sort(r_intList10000);

        //////////////////////////////////////////////////////////////////////////////

        intList100 = ValueGenerator.generateIntegers(100);
        intList1000 = ValueGenerator.generateIntegers(1000);
        intList10000 = ValueGenerator.generateIntegers(10000);

        intList9 = ValueGenerator.generateSpecificIntegerList(1000, 9);
        intList0 = ValueGenerator.generateSpecificIntegerList(1000, 0);
        intList3 = ValueGenerator.generateSpecificIntegerList(1000, 3);

        r_intList100 = ValueGenerator.reverseTheList(intList100);
        r_intList1000 = ValueGenerator.reverseTheList(intList1000);
        r_intList10000 = ValueGenerator.reverseTheList(intList10000);

        SelectSort selectSort = new SelectSort();
        selectSort.sort(intList100);
        selectSort.sort(intList1000);
        selectSort.sort(intList10000);
        selectSort.sort(intList9);
        selectSort.sort(intList0);
        selectSort.sort(intList3);
        selectSort.sort(r_intList100);
        selectSort.sort(r_intList1000);
        selectSort.sort(r_intList10000);
    }
}
