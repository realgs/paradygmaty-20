package SortingMethods;
import java.io.*;
import java.util.*;

public class Sortowanie {
    public static ArrayList<Integer> zbior;
    public static ArrayList<Integer> zbior_cocktail;
    public static ArrayList<Integer> zbior_comb;
    public static ArrayList<Integer> zbiorPosortowany;


    public static ArrayList<Integer> createArrayList(File f) throws IOException {
        ArrayList<Integer> zbior1 = new ArrayList<>();
        BufferedReader br = new BufferedReader(new FileReader(f));
        String st;

        String st2 = "";
        try{
            while(((st = br.readLine()) != null)){
                st2 = st2 + st + ",";
            }
        }catch(Exception e){
            e.printStackTrace();
        }
        br.close();

        String str[] = st2.split(",");

        for(int i = 0; i < str.length; i++){
            zbior1.add(Integer.parseInt(str[i]));
        }

        zbior = zbior1;

        return zbior1;

    }

    public static ArrayList<Integer> createArrayList_cocktail(File f) throws IOException{
        ArrayList<Integer> zb1 = new ArrayList<>();
        zb1 = createArrayList(f);
        zbior_cocktail = zb1;
        return zb1;

    }

    public static ArrayList<Integer> createArrayList_comb(File f) throws IOException{
        ArrayList<Integer> zb1 = new ArrayList<>();
        zb1 = createArrayList(f);
        zbior_comb = zb1;
        return zb1;
    }

    //z1 cocktailsort
    public static String sortowanie(ArrayList<Integer> ar1) {
        ar1 = zbior_cocktail;
        System.out.println("-------------------------------------------------------------");
        System.out.println("Cocktail sort");
        System.out.println("Zbior wejsciowy: ");
        for(int i : ar1){
            System.out.print(i + ",");
        }
        System.out.println();

        int liczbaPorownan = 0;
        int liczbaZamian = 0;

        boolean swapped = true;

        while(swapped == true){
            swapped = false;

            for(int i = 0; i < ar1.size() - 1; i++){
                liczbaPorownan++;
                if (ar1.get(i) > ar1.get(i + 1)) {
                    int temp = ar1.get(i);
                    ar1.set(i, ar1.get(i+1));
                    ar1.set(i+1, temp);
                    liczbaZamian++;
                    swapped = true;
                }
            }

            if(swapped == false){
                break;
            }
            else{
                swapped =false;
            }

            int end = ar1.size() - 1;

            for(int i = end - 1; i >= 0; i--){
                if (ar1.get(i) > ar1.get(i + 1)) {
                    int temp = ar1.get(i);
                    ar1.set(i, ar1.get(i+1));
                    ar1.set(i+1, temp);
                    swapped = true;
                }
            }

        }
        System.out.println();
        System.out.println("Zbior posortowany: ");
        for(int i : ar1){
            System.out.print(i + ", ");
        }
        zbiorPosortowany = ar1;

        System.out.println();
        System.out.println("Liczba zamian: " + liczbaZamian);
        System.out.println("Liczba porownan: " + liczbaPorownan);
        System.out.println();
        System.out.println();
        String result = "Liczebność zbioru: " + ar1.size() + " CocktailSort \t porównania: " + liczbaPorownan + "\t zamiany: " + liczbaZamian;

        return result;
    }


    public int liczebnosc(ArrayList<Integer> ar1){
        ar1 = zbior_cocktail;
        ListIterator it = ar1.listIterator();
        int counter = 0;
        while(it.hasNext()){
            if(it.next() != null){
                counter++;
            }
            else{
                break;
            }
        }

        System.out.println("Liczebnosc zbioru: " + counter);

        return counter;
    }


    public static int max(ArrayList<Integer> ar1){
        ar1 = zbiorPosortowany;
        int max = ar1.get(ar1.size() - 1);
        System.out.println("Maximum: " + max);
        return max;
    }


    public static int min(ArrayList<Integer> ar1){
        ar1 = zbiorPosortowany;
        int min = ar1.get(0);
        System.out.println("Minimum: " + min);
        return min;
    }


    public static  double mediana(ArrayList<Integer> ar1){
        ar1 = zbiorPosortowany;
        int mediana = 0;
        int count = 0;
        for(int i : ar1){
            count += i;
        }

        mediana = count / ar1.size();
        System.out.println("Wartosc srodkowa: " + mediana);

        return mediana;
    }

    //z2

    //wspolczynnik
    public static int getNextOne(int item){
        item = (item * 10) / 13;
        if(item < 1){
            return 1;
        }
        return item;
    }

    //combSort
    public static String combSort(ArrayList<Integer> ar2){
        ar2 = zbior_comb;
        System.out.println("-------------------------------------------------------------");
        System.out.println("Comb sort");
        System.out.println("Zbior wejsciowy: ");
        for(int i : ar2){
            System.out.print(i + ",");
        }
        System.out.println();

        int liczbaPorownan = 0;
        int liczbaZamian = 0;
        int n = ar2.size();
        int item = n;
        boolean swapped = true;

        while(item != 1 || swapped == true){
            //szukamy nastepna czesc
            item = getNextOne(item);

            swapped = false;
            for (int i = 0; i < n - item; i++) {
                liczbaPorownan++;
                if(ar2.get(i) > ar2.get(i + item)){
                    int temp = ar2.get(i);
                    ar2.set(i, ar2.get(i + item));
                    ar2.set(i + item, temp);
                    swapped = true;
                    liczbaZamian++;
                }
            }
        }

        System.out.println();
        System.out.println("Zbior posortowany: ");
        for(int i : ar2){
            System.out.print(i + ", ");
        }
        System.out.println("\n");
        System.out.println();
        System.out.println("Liczba zamian: " + liczbaZamian);
        System.out.println("Liczba porownan: " + liczbaPorownan);
        System.out.println();

        String result = "Liczebność zbioru: " + ar2.size() +" CombSort \t porównania: " + liczbaPorownan + "\t zamiany: " + liczbaZamian;

        return result;
    }


    public static void main(String[] args) throws IOException {
        //zad1
        /*    File file = new File("D:\\prog\\Java\\AiSD\\src\\lab01\\z1data1.txt");
        ArrayList<Integer> ar1 = createArrayList(file);
        sortowanie(ar1);
        liczebnosc(ar1);
        max(ar1);
        min(ar1);
        mediana(ar1);

        File file2 = new File("D:\\prog\\Java\\AiSD\\src\\lab01\\z1data2.txt");
        ArrayList<Integer> ar2 = createArrayList(file2);
        sortowanie(ar2);
        liczebnosc(ar2);
        max(ar2);
        min(ar2);
        mediana(ar2);

        File output = new File("output.txt");
        output.createNewFile();

        FileWriter fr = new FileWriter(output);
        for(int i : ar2){
            fr.write(i + ",");
        }
        fr.flush();
        fr.close();
        */
        //koniec zad1

        //zad2

        //inicjacja danych wejsciowych

        File z2data11 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data11.csv");
        File z2data12 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data12.csv");
        File z2data13 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data13.csv");
        File z2data21 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data21.csv");
        File z2data22 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data22.csv");
        File z2data23 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data23.csv");
        File z2data31 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data31.csv");
        File z2data32 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data32.csv");
        File z2data33 = new File("D:\\PWr\\3rok\\ParadygmatyLab\\paradygmaty-20\\ParadygmatyLaboratoria\\src\\SortingMethods\\z2data33.csv");


        //data11
        System.out.println("data11  \n");
        ArrayList<Integer> d11Cocktail = createArrayList_cocktail(z2data11);
        ArrayList<Integer> d11Comb = createArrayList_comb(z2data11);
        String s1 =sortowanie(d11Cocktail);
        String s2 = combSort(d11Comb);

        //data12
        System.out.println("data12  \n");
        ArrayList<Integer> d12Cocktail = createArrayList_cocktail(z2data12);
        ArrayList<Integer> d12Comb = createArrayList_comb(z2data12);
        String s3 = sortowanie(d12Cocktail);
        String s4 = combSort(d12Comb);

        //data13
        System.out.println("data13  \n");
        ArrayList<Integer> d13Cocktail = createArrayList_cocktail(z2data13);
        ArrayList<Integer> d13Comb = createArrayList_comb(z2data13);
        String s5 = sortowanie(d13Cocktail);
        String s6 = combSort(d13Comb);

        //data21
        System.out.println("data21  \n");
        ArrayList<Integer> d21Cocktail = createArrayList_cocktail(z2data21);
        ArrayList<Integer> d21Comb = createArrayList_comb(z2data21);
        String s7 = sortowanie(d21Cocktail);
        String s8 = combSort(d21Comb);

        //data22
        System.out.println("data22  \n");
        ArrayList<Integer> d22Cocktail = createArrayList_cocktail(z2data22);
        ArrayList<Integer> d22Comb = createArrayList_comb(z2data22);
        String s9 = sortowanie(d22Cocktail);
        String s10 = combSort(d22Comb);

        //data23
        System.out.println("data23  \n");
        ArrayList<Integer> d23Cocktail = createArrayList_cocktail(z2data23);
        ArrayList<Integer> d23Comb = createArrayList_comb(z2data23);
        String s11 = sortowanie(d23Cocktail);
        String s12 = combSort(d23Comb);

        //data31
        System.out.println("data31  \n");
        ArrayList<Integer> d31Cocktail = createArrayList_cocktail(z2data31);
        ArrayList<Integer> d31Comb = createArrayList_comb(z2data31);
        String s13 =sortowanie(d31Cocktail);
        String s14 = combSort(d31Comb);

        //data32
        System.out.println("data32  \n");
        ArrayList<Integer> d32Cocktail = createArrayList_cocktail(z2data32);
        ArrayList<Integer> d32Comb = createArrayList_comb(z2data32);
        String s15 = sortowanie(d32Cocktail);
        String s16 = combSort(d32Comb);

        //data33
        System.out.println("data33  \n");
        ArrayList<Integer> d33Cocktail = createArrayList_cocktail(z2data33);
        ArrayList<Integer> d33Comb = createArrayList_comb(z2data33);
        String s17 = sortowanie(d33Cocktail);
        String s18 = combSort(d33Comb);

        File output = new File("output.txt");
        output.createNewFile();

        String[] results = new String[18];
        results[0] = s1;
        results[1] = s2;
        results[2] = s3;
        results[3] = s4;
        results[4] = s5;
        results[5] = s6;
        results[6] = s7;
        results[7] = s8;
        results[8] = s9;
        results[9] = s10;
        results[10] = s11;
        results[11] = s12;
        results[12] = s13;
        results[13] = s14;
        results[14] = s15;
        results[15] = s16;
        results[16] = s17;
        results[17] = s18;


        FileWriter fr = new FileWriter(output);
        String[] fileNames = new String[18];
        fileNames[0] = "z2data11.csv";
        fileNames[1] = "z2data11.csv";
        fileNames[2] = "z2data12.csv";
        fileNames[3] = "z2data12.csv";
        fileNames[4] = "z2data13.csv";
        fileNames[5] = "z2data13.csv";
        fileNames[6] = "z2data21.csv";
        fileNames[7] = "z2data21.csv";
        fileNames[8] = "z2data22.csv";
        fileNames[9] = "z2data22.csv";
        fileNames[10] = "z2data23.csv";
        fileNames[11] = "z2data23.csv";
        fileNames[12] = "z2data31.csv";
        fileNames[13] = "z2data31.csv";
        fileNames[14] = "z2data32.csv";
        fileNames[15] = "z2data32.csv";
        fileNames[16] = "z2data33.csv";
        fileNames[17] = "z2data33.csv";

        for(int i =0; i < 18; i++) {
            fr.write(fileNames[i] + "\t" + results[i] + "\n");
        }

        String text = "\n1.Wplyw liczebnosci zbioru. CombSort okazuje sie lepszy w przypadku mniejszej liczebnosci zbioru" +
                "\n 2.Wplyw rodzaju danych wejsciowych. CombSort okazuje sie bardziej skuteczny w przypadku nieposortowanego\n zbioru danych wejsciowych, wskazuje na to znacznie mniejsza liczba porownan i zamian. " +
                "\n 3.Problemy implementacji. W mojej implementacji odczytywanie plikow jest zrealizowane przy pomocy klasy String.\n Przy odpowiednio duzych ilosciach danych String nie pozwoli na zamieszczenie takiej ilosci danych." +
                "\n Takze, jest niewygodna realizacja dodania nowego przypadku sortowania " ;

        fr.write(text);

        fr.flush();
        fr.close();
    }
}
