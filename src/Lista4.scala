import scala.util.Random

object Lista4 extends App {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val maxNodeVal = 5


  // zad 1 3pkt
  def generujDrzewo[A](levels: Int): BT[Int] =
    if (levels <= 0) Node(Random.nextInt(maxNodeVal) + 1, Empty, Empty)
    else Node(Random.nextInt(maxNodeVal) + 1, generujDrzewo(levels - 1), generujDrzewo(levels - 1))

  println("\nZad 1")
  println(generujDrzewo(2))

  // zad 2 3pkt
  def odejmijDrzewo[A](tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    (tree1, tree2) match {
      case (Node(val1, Empty, Empty), Node(val2, Empty, Empty)) => Node(val1 - val2, Empty, Empty)
      case (Node(_, _, Empty), Node(_, _, _)) => throw new Exception("Drzewo 1 mniejsze od drzewa 2 lub drzewa nieregularne")
      case (Node(_, Empty, _), Node(_, _, _)) => throw new Exception("Drzewo 1 mniejsze od drzewa 2 lub drzewa nieregularne")
      case (Node(_, _, _), Node(_, _, Empty)) => throw new Exception("Drzewo 2 mniejsze od drzewa 1 lub drzewa nieregularne")
      case (Node(_, _, _), Node(_, Empty, _)) => throw new Exception("Drzewo 2 mniejsze od drzewa 1 lub drzewa nieregularne")
      case (Node(val1, left1, right1), Node(val2, left2, right2)) => Node(val1 - val2, odejmijDrzewo(left1, left2), odejmijDrzewo(right1, right2))
    }

  println("\nZad 2")
  val tree1 = generujDrzewo(1)
  val tree2 = generujDrzewo(1)
  println(tree1)
  println(tree2)
  println(odejmijDrzewo(tree1, tree2))

  val tree3 = generujDrzewo(1)
  val tree4 = generujDrzewo(2)
  println(tree3)
  println(tree4)
  //println(odejmijDrzewo(tree3, tree4)) //these test throws an exception as planned

  // zad 3
  // przejscie w glab 1 pkt
  def removeInDepth[A](tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) =
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (_, Empty) => throw new Exception("Drzewo 1 mniejsze od drzewa 2 lub drzewa nieregularne")
      case (Empty, _) => throw new Exception("Drzewo 2 mniejsze od drzewa 1 lub drzewa nieregularne")
      case (Node(val1, left1, right1), Node(val2, left2, right2)) =>

        val leftResult = removeInDepth(left1, left2)
        val rightResult = removeInDepth(right1, right2)

        if (val1 == val2)
          if (leftResult == (Empty, Empty) && rightResult == (Empty, Empty)) (Empty, Empty)
          else (Node(-1, leftResult._1, rightResult._1), Node(-1, leftResult._2, rightResult._2))
        else (Node(val1, leftResult._1, rightResult._1), Node(val2, leftResult._2, rightResult._2))
    }

  println("\nZad 3")
  println(removeInDepth(tree1, tree2))
  println(removeInDepth(tree1, tree3))
  println(removeInDepth(tree2, tree3))
  println(removeInDepth(tree1, tree1))
  //println(removeInDepth(tree1, tree4)) //these test throws an exception as planned

  // funkcja pomocnicza do LazyList
  def nLazyListElemsToList[A](lazyLista: LazyList[A], n: Int): List[A] =
    if (n > 0 && lazyLista != LazyList())
      lazyLista.head :: nLazyListElemsToList(lazyLista.tail, n - 1)
    else Nil

  // zad 4 5pkt
  def eachNElement[A](lista: LazyList[A], m: Int, n: Int): LazyList[A] = {
    def eachNElementBody(lista: LazyList[A], a: Int, b: Int): LazyList[A] = {
      if (lista == LazyList()) LazyList()
      else (a, b) match {
        case (0, 1) => LazyList(lista.head)
        case (_, 1) => LazyList()
        case (0, _) => lista.head #:: eachNElementBody(lista.tail, m - 1, b - 1)
        case (_, _) => eachNElementBody(lista.tail, a - 1, b - 1)
      }
    }

    if (m < 1 || n < 1) throw new Exception("Zle parametry wejsciowe")
    else if (lista == LazyList()) LazyList()
    else lista.head #:: eachNElementBody(lista.tail, m - 1, n - 1)
  }

  println("\nZad 4")
  println(nLazyListElemsToList(eachNElement(LazyList(), 2, 3), 20))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 3), 20))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 4), 20))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 7), 20))
  println(nLazyListElemsToList(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3, 20), 20))

  // zad 5 5pkt

  val dodajInt = (x1: Int) => (x2: Int) => x1 + x2
  val odejmijInt = (x1: Int) => (x2: Int) => x1 - x2
  val pomnozInt = (x1: Int) => (x2: Int) => x1 * x2
  val podzielInt = (x1: Int) => (x2: Int) => x1 + x2

  def ldzialanie[A](lista1: LazyList[A], lista2: LazyList[A], operacja: A => A => A): LazyList[A] =
    if (lista1 == LazyList()) lista2
    else if (lista2 == LazyList()) lista1
    else operacja(lista1.head)(lista2.head) #:: ldzialanie(lista1.tail, lista2.tail, operacja)

  println("\nZad 5")
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), dodajInt), 20))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), odejmijInt), 20))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), pomnozInt), 20))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4), podzielInt), 20))
  println(nLazyListElemsToList(ldzialanie(LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4, 5, 6), dodajInt), 20))


}
