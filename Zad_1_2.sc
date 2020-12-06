import scala.collection.immutable.TreeSet

class zad1_2 {
  //Zad 1 (2.5 pkt)
  def duplicate(list_elems: List[Int], elem_reiteration: List[Int]): List[Int] = {
    def duplicate_elem(elem: Int, count: Int): List[Int] = {
      if (count != 0) {
        elem :: duplicate_elem(elem, count - 1)
      }
      else {
        duplicate(list_elems.tail, elem_reiteration.tail)
      }
    }

    (list_elems, elem_reiteration) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_, _) => if (elem_reiteration.head != 0) {
        list_elems.head :: duplicate_elem(list_elems.head, elem_reiteration.head - 1)
      }
      else {
        duplicate(list_elems.tail, elem_reiteration.tail)
      }
    }

  }

  println(duplicate(List(1, 2, 3), List(0, 3, 1, 4)))
  println(duplicate(Nil, List(0, 3, 2, 4)))
  println(duplicate(List(1, 2, 3, 0), Nil))
  println(duplicate(Nil, Nil))
  println(duplicate(List(1, 2, 3, 0), List(0)))
  println(duplicate(List(0, 1, 2, 3, 4, 5), List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(duplicate(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(0, 1, 2, 3)))

  //Zad 2 (2.5 pkt)
  //Wykorzystałem Set ponieważ nie zawieraja zduplikowanych elementów
  def no_duplicate(list: Set[Int], elem_reiteration: List[Int]): List[Int] = {
    def duplicate_elem(elem: Int, count: Int): List[Int] = {
      if (count != 0) {
        elem :: duplicate_elem(elem, count - 1)
      }
      else {
        no_duplicate(list.tail, elem_reiteration.tail)
      }
    }

    (list.toList, elem_reiteration) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_, _) => if (elem_reiteration.head != 0) {
        list.head :: duplicate_elem(list.head, elem_reiteration.head - 1)
      }
      else {
        no_duplicate(list.tail, elem_reiteration.tail)
      }
    }

  }

  // Wykorzystałem TreeSet, dlatego że np. Set(0,1,2,3,4,5,0,1,2,3,4,5), daje na wyjściu
  //HashSet(0, 5, 1, 2, 3, 4) i w programie będzie przetwarzany w kolejności która jest
  // niepoprawna, poniważ chcemy mieć (0,1,2,3,4,5),
  //a TreeSet(0,1,2,3,4,5,0,1,2,3,4,5), daje na wyjsciu TreeSet(0, 1, 2, 3, 4, 5),
  //co pozwala nam na zachowanie początkowej kolejnośći

  println("With no duplicates")
  println(no_duplicate(TreeSet(1, 2, 3, 1), List(0, 3, 1, 4)))
  println(no_duplicate(TreeSet(), List(0, 3, 2, 4)))
  println(no_duplicate(TreeSet(1, 2, 3, 0), Nil))
  println(no_duplicate(TreeSet(), Nil))
  println(no_duplicate(TreeSet(1, 2, 3, 0), List(0)))
  println(no_duplicate(TreeSet(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5), List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(no_duplicate(TreeSet(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 5), List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))

}

val obj = new zad1_2



