public class Main {
    public static void main(String[] args) {
        Sort s = new Sort(10);
        int [] array = s.generateArray(20);
        s.displayArray();
        s.bubbleSort(array);
        s.removeDuplicate(array);
        
        
        Sort s2 = new Sort(10);
        int [] array2 = s2.generateArray(20);
        s2.displayArray();
        s2.bubbleSort(array2);
        s2.removeDuplicate(array2);
    }
}
