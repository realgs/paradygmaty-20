class BinaryTrees {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Leaf[+A](value: A) extends BT[A]
  case class Node[+A](value: A, left: BT[A], right: BT[A]) extends BT[A]

  //Funkcje do testów

  //funkcja sprawdzająca głębokość drzewa
  def maxDepth [A] (root: BT[A]): Int = {
    root match {
      case Node(_, l, r) => Math.max(maxDepth(l), maxDepth(r)) + 1
      case _ => 0
    }
  }

  //funkcja licząca liście drzewa
  def countLeafs [A] (root: BT[A]): Int = {
    root match {
      case Leaf(_) => 1
      case Node(_, left, right) => countLeafs(left) + countLeafs(right)
      case Empty => 0
    }
  }

  //funkcja zwracająca wartość korzenia drzewa
  def rootValue [A] (root: BT[A]): A = {
    root match {
      case Leaf(value) => value
      case Node(value, _, _) => value
      case Empty => throw new IllegalArgumentException
    }
  }

  /*  1) Napisz funkcję generującą drzewo o głębokości N (liczba poziomów drzewa), zawierającą losowe,
  dodatnie liczby całkowite z zadanego zakresu. Drzewo pełne. Proszę o testy(!)
    Punkty: 3 */

  def treeGenerator (depth: Int, range: Int): BT[Int] =
    if(depth < 0 || range < 1) throw new IllegalArgumentException
    else {
      depth match {
        case 0 => Leaf(util.Random.nextInt(range) + 1)
        case _ => Node(util.Random.nextInt(range) + 1, treeGenerator(depth - 1, range), treeGenerator(depth - 1, range))
      }
    }

 /* 2) Napisz funkcję przyjmującą na wejściu dwa drzewa o tej samej głębokości wygenerowane przy użyciu powyższej
 funkcji i zwracającą drzewo składające się z różnicy ich elementów. Zwróć uwagę na wydajność rozwiązania.
    Punkty: 3 */

  def treesSubtraction (firstT: BT[Int], secondT: BT[Int]): BT[Int] = {
    (firstT, secondT) match {
      case (Leaf(valF), Leaf(valS)) => Leaf(valF - valS)
      case (Node(valF, lF, rF), Node(valS, lS, rS)) => Node(valF - valS, treesSubtraction(lF, lS), treesSubtraction(rF, rS))
      case (_, _) => throw new IllegalArgumentException
    }
  }

 /* 3) Napisz funkcję przyjmującą dwa pełne drzewa o tej samej głębokości i usuwającą powtarzające się wartości w
 określony sposób: jeśli wartość węzła i wszystkich jego potomków jest identyczna - usuń całą gałąź.
 Jeśli wartość węzła jest identyczna, a któryś z potomków się różni wstaw -1, potomków zostaw bez zmian.
 Funkcja ma zwracać dwa drzewa z usuniętymi w powyższy sposób duplikatami. Zaimplementuj przechodząc po
 drzewie wgłąb (Punkty: 1) i wszerz (Punkty: 3). Zwróć uwagę na wydajność rozwiązania.*/

  def deleteCopiesDFS(firstT: BT[Int], secondT: BT[Int]): (BT[Int], BT[Int]) = {
    (firstT, secondT) match {
      case (Leaf(valF), Leaf(valS)) => {
        if (valF == valS) (Leaf(-1), Leaf(-1))
        else (Leaf(valF), Leaf(valS))
      }
      case (Node(valF, lF, rF), Node(valS, lS, rS)) => {
        val deleteLeft = deleteCopiesDFS(lF, lS)
        val deleteRight = deleteCopiesDFS(rF, rS)
        if(valF != valS)
          (Node(valF, deleteLeft._1, deleteRight._1), Node(valS, deleteLeft._2, deleteRight._2))
        else if (equalsDFS(firstT, secondT))
          (Empty, Empty)
        else
          (Node(-1, deleteLeft._1, deleteRight._1), Node(-1, deleteLeft._2, deleteRight._2))
      }
      case (_, _) => throw new IllegalArgumentException
    }
  }

  def equalsDFS (firstT: BT[Int], secondT: BT[Int]): Boolean = {
    (firstT, secondT) match {
      case (Leaf(vF), Leaf(vS)) => vF == vS
      case (Node(valF, lF, rF), Node(valS, lS, rS)) => valF == valS & equalsDFS(lF, lS) & equalsDFS(rF, rS)
      case (_, _) => false
    }
  }

  def deleteCopiesBFS (firstT: BT[Int], secondT: BT[Int]): (BT[Int], BT[Int]) = {
    (firstT, secondT) match {
      case (Leaf(valF), Leaf(valS)) => {
        if (valF == valS) (Leaf(-1), Leaf(-1))
        else (Leaf(valF), Leaf(valS))
      }
      case (Node(valF, lF, rF), Node(valS, lS, rS)) => {
        if (valF != valS)
          (Node(valF, deleteCopiesBFS(lF, lS)._1, deleteCopiesBFS(rF, rS)._1), Node(valS, deleteCopiesBFS(lF, lS)._2, deleteCopiesBFS(rF, rS)._2))
        else {
          val ifLeftSubtreesEquals = equalsBFS(lF, lS)
          val ifRightSubtreesEquals = equalsBFS(rF, rS)
          (ifLeftSubtreesEquals, ifRightSubtreesEquals) match {
            case (true, true) => (Empty, Empty)
            case (true, false) => (Node(-1, Empty, deleteCopiesBFS(rF, rS)._1), Node(-1, Empty, deleteCopiesBFS(rF, rS)._2))
            case (false, true) => (Node(-1, deleteCopiesBFS(lF, lS)._1, Empty), Node(-1, deleteCopiesBFS(lF, lS)._2,Empty))
            case (false, false) => (Node(-1, deleteCopiesBFS(lF, lS)._1, deleteCopiesBFS(rF, rS)._1), Node(-1, deleteCopiesBFS(lF, lS)._2, deleteCopiesBFS(rF, rS)._2))
          }
        }
      }
      case (_, _) => throw new IllegalArgumentException
    }
  }

  def equalsBFS (firstT: BT[Int], secondT: BT[Int]): Boolean = {
    def helper(queueF: List[BT[Int]], queueS: List[BT[Int]]): Boolean =
      (queueF, queueS) match {
        case (Nil, Nil) => true
        case (Leaf(valF) :: tF, Leaf(valS) :: tS) => valF == valS & helper(tF, tS)
        case (Node(valF, lF, rF) :: tF, Node(valS, lS, rS) :: tS) =>
          valF == valS & helper(tF ::: List(lF, rF), tS ::: List(lS, rS))
        case (_, _) => false
      }
    helper (List(firstT), List(secondT))
  }


}
