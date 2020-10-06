import java.util.ArrayList;
import java.util.Random;

public class ValueGenerator
{
    public static ArrayList<Integer> generateIntegers(int amount, int min, int max)
    {
        ArrayList<Integer> list = new ArrayList<>();

        Random r = new Random();
        for(int i = 0 ; i < amount ; i++)
        {
            list.add(r.nextInt((max - min) + 1) + min);
        }
        return list;
    }

    public static ArrayList<Integer> generateIntegers(int amount)
    {
        ArrayList<Integer> list = new ArrayList<>();

        Random r = new Random();
        for(int i = 0 ; i < amount ; i++)
        {
            list.add(r.nextInt((amount - 1) + 1) + 1);
        }
        return list;
    }

    public static ArrayList<Integer> generateSpecificIntegerList(int amount, int number){
        ArrayList<Integer> list = new ArrayList<>();

        for(int i = 0 ; i < amount ; i++)
        {
            list.add(number);
        }
        return list;
    }

    public static ArrayList<Integer> reverseTheList(ArrayList<Integer> list){
        ArrayList<Integer> r_list = new ArrayList<>();

        for(int i = list.size() - 1 ; i >= 0 ; i--)
        {
            r_list.add(list.get(i));
        }
        return r_list;
    }
}
