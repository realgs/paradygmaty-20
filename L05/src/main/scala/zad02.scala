import scala.collection.immutable.HashSet

/*
 * Zadanie 2 (2.5 pkt)
 *
 * Jako reprezentację kolekcji wybrałem listę ponieważ operacje head, tail oraz
 * prepend (::) zajmują O(1) czasu a więcej operacji nam w zasadzie nie potrzeba.
 *
 * Przyjmując, że rozmiar kolekcji zawierającej elementy do powielenia wynosi N1
 * oraz suma pierwszych N1 krotności z drugiej kolekcji równa się F to
 * złożoność obliczeniowa poniższej metody wynosi O(2F + N1) ~ O(n).
 * Dodajemy N1 ponieważ na początku musimy sprawdzić czy w pierwszej kolekcji nie
 * znajdują się żadne duplikaty.
 * Mnożymy przez 2 ponieważ zanim zwrócimy końcową listę musimy ją jeszcze odwrócić.
 * Można się pozbyć tego mnożnika stosując zwykłą rekurencję zamiast ogonowej i w
 * rezultacie nie odwracać listy ale wtedy pozbawiamy się optymalizacji stosu.
 */
object zad02 {
  def duplicate[A](toDuplicate: List[A], factors: List[Int]): List[A] = {
    if (!areElementsDistinct(toDuplicate, HashSet()))
      throw new IllegalArgumentException("Elements to duplicate should be distinct")
    else zad01.duplicate(toDuplicate, factors)
  }

  @scala.annotation.tailrec
  private def areElementsDistinct[A](elementsToCheck: List[A], set: HashSet[A]): Boolean = {
    elementsToCheck match {
      case Nil => true
      case head :: tail =>
        if (set.contains(head)) false
        else areElementsDistinct(tail, set + head)
    }
  }
}
