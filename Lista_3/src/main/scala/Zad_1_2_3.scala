class Zad_1_2_3 {

  /*1) Napisz funkcję tworzącą z listy wejściowej dwie listy. W pierwszej mają się znaleźć wszystkie elementy
  o wartościach ujemnych. W drugiej mają się znaleźć wszystkie elementy o wartościach ujemnych,
  nieparzystych (każda liczba ma spełniać oba te warunki). Porządek elementów musi być zachowany.
  Elementy w listach mogą (i będą) się powielać. Wynik zwróć w postaci pary list.*/

  def findNegativeNumbers(list: List[Int]): (List[Int], List[Int]) =
    list match {
      case Nil => (Nil, Nil)
      case hd :: tl if hd < 0 && hd % 2 != 0 => val (xs1, xs2) = findNegativeNumbers(tl); (hd :: xs1, hd :: xs2)
      case hd :: tl if hd < 0 && hd % 2 == 0 => val (xs1, xs2) = findNegativeNumbers(tl); (hd :: xs1, xs2)
      case hd :: tl if hd >= 0 => val (xs1, xs2) = findNegativeNumbers(tl); (xs1, xs2)
    }

  /*2) Napisz funkcję zwracającą długość dowolnej listy. Funkcja ma mieć jeden parametr.
      Jaką złożoności obliczeniową i pamięciową udało Ci się osiągnąć? */

  def findLength[A](list: List[A]): Int =
    if (list == Nil) 0
    else 1 + findLength(list.tail)

  /* 3) Napisz funkcję łączącą dwie podane listy. Elementy w liście wyjściowej mają występować naprzemiennie.
     Jaką złożoności obliczeniową i pamięciową udało Ci się osiągnąć?*/

  def mixLists [A](listA: List[A], listB: List[A]): List[A] =
    (listA, listB) match {
      case (h1 :: t1, h2 :: t2) => h1 :: h2 :: mixLists(t1, t2)
      case (Nil, h2 :: t2) => h2 :: mixLists(Nil, t2)
      case (h1 :: t1, Nil) => h1 :: mixLists(t1, Nil)
      case (Nil, Nil) => Nil
    }

}
