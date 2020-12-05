/*
 * Zadanie 1 (2.5 pkt)
 *
 * Jako reprezentację kolekcji wybrałem listę ponieważ operacje head, tail oraz
 * prepend (::) zajmują O(1) czasu a więcej operacji nam w zasadzie nie potrzeba.
 *
 * Przyjmując, że rozmiar kolekcji zawierającej elementy do powielenia wynosi N1
 * oraz suma pierwszych N1 krotności z drugiej kolekcji równa się F to
 * złożoność obliczeniowa poniższej metody wynosi O(2F) ~ O(n).
 * Mnożymy przez 2 ponieważ zanim zwrócimy końcową listę musimy ją jeszcze odwrócić.
 * Można się pozbyć tego mnożnika stosując zwykłą rekurencję zamiast ogonowej i w
 * rezultacie nie odwracać listy ale wtedy pozbawiamy się optymalizacji stosu.
 */
object zad01 {
  def duplicate[A](toDuplicate: List[A], factors: List[Int]): List[A] = {
    duplicate(toDuplicate, factors, List()).reverse
  }

  @scala.annotation.tailrec
  private def duplicate[A](toDuplicate: List[A], factors: List[Int], result: List[A]): List[A] = {
    (toDuplicate, factors) match {
      case (Nil, _) => result
      case (head :: tail, Nil) => duplicate(tail, Nil, head :: result)
      case (head :: tail, factor :: ftail) =>
        if (factor < 1) duplicate(tail, ftail, result)
        else if (factor == 1) duplicate(tail, ftail, head :: result)
        else duplicate(toDuplicate, factor - 1 :: ftail, head :: result)
    }
  }
}
