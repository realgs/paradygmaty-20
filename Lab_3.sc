class Lab_3 {
  def podziel(list: List[Int]): (List[Int], List[Int]) = {
    def pom1(list: List[Int]): List[Int] =
      if (list == Nil) Nil
      else if (list.head < 0) (list.head :: pom1(list.tail))
      else pom1(list.tail)

    def pom2(list: List[Int]): List[Int] =
      if (list == Nil) Nil
      else if (list.head < 0 && list.head % 2 != 0) (list.head :: pom2(list.tail))
      else pom2(list.tail)

    if (list == Nil) (Nil, Nil)
    else
      (pom1(list), pom2(list))
  }

  def dlugosc(list: List[Int]): Int = {
    if (list == Nil) 0
    else dlugosc(list.tail) + 1
  }

  def polacz(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list1 == Nil && list2 == Nil) (Nil)
    else if (list1 != Nil && list2 != Nil) {
      list1.head :: list2.head :: polacz(list1.tail, list2.tail)
    }
    else if (list1 != Nil && list2 == Nil)
      list1.head :: polacz(list1.tail, list2)
    else
      list2.head :: polacz(list1, list2.tail)
  }

  def find(list: List[String], word: String): List[String] = {
    def contains(word: String, word_find: String): Boolean = {
      if (word == "") false
      else if (word_find == "") true
      else if (word.head == word_find.head)
        contains(word.tail, word_find.tail)
      else
        contains(word.tail, word_find)
    }

    if (list == Nil) Nil
    else {
      if (contains(list.head, word) == true)
        list.head :: find(list.tail, word)
      else
        find(list.tail, word)
    }
  }


  def find_N(list: List[String], word_list: List[String]): List[String] = {
    if (list == Nil || word_list == Nil) Nil
    else
      find(list, word_list.head) ::: find_N(list, word_list.tail)
  }

  def joinLists(list1: List[Int], list2: List[Int], list3: List[Int]): List[Int] = {
    if (list1 == Nil && list2 == Nil && list3 == Nil) Nil

    else if (list1 != Nil)
      list1.head :: joinLists(list1.tail, list2, list3)

    else if (list1 == Nil && list2 != Nil)
      list2.head :: joinLists(list1, list2.tail, list3)

    else
      list3.head :: joinLists(list1, list2, list3.tail)
  }
}

val lab = new Lab_3;

val arr1 = List(-3, -6, 8, -9, 13)
val arr2 = List(-1, 23, 0, -12, -7, 100, -27)
val arr3 = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
print(lab.podziel(arr1))
print(lab.podziel(arr2))
print(lab.podziel(arr3))

val arr1_d = List(-3, -6, 8, -9, 13)
val arr2_d = List(-1, 23, 0, -12, -7, 100, -27)
val arr3_d = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
print(lab.dlugosc(arr1_d))
print(lab.dlugosc(arr2_d))
print(lab.dlugosc(arr3_d))

val arr1_p = List(-3, -6, 8, -9, 13)
val arr2_p = List(-1, 23, 0, -12, -7, 100, -27)
val arr3_p = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
val arr4_p = Nil
print(lab.polacz(arr1_p, arr2_p))
print(lab.polacz(arr4_p, arr2_p))
print(lab.polacz(arr3_p, arr4_p))

val find_word = "index0168";
val arr1_f = List("index0169", "zindex0168202", "index0168211", "index0168210", "index0169222", "index0169224")
print(lab.find(arr1_f, find_word))

val word_list = List("index0168", "0169");
val word_nil = Nil
val arr1_fN = List("index0169", "zindex0168202", "index0168211", "index0168210", "index0169222", "index0169224")
print(lab.find_N(arr1_fN, word_list))
print(lab.find_N(arr1_fN, word_nil))

val arr1_j = List(5, 4, 3, 2)
val arr2_j = List(1, 0)
val arr3_j = List(9)
val arr_nil = Nil

print(lab.joinLists(arr1_j, arr2_j, arr3_j))
print(lab.joinLists(arr1_j, arr_nil, arr3_j))
print(lab.joinLists(arr_nil, arr_nil, arr_nil))