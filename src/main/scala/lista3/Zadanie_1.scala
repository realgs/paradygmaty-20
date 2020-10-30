package lista3

object Zadanie_1 {
  def podziel(list: List[Int]): (List[Int], List[Int]) =
    (filter(list, (n: Int) => n < 0), filter(list, (n: Int) => n < 0 && n % 2 != 0))

  def filter(list: List[Int], predic: Int => Boolean): List[Int] =
    list match {
      case Nil => Nil
      case _ => if (predic(list.head)) list.head :: filter(list.tail, predic) else filter(list.tail, predic)
    }
}
