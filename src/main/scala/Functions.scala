import scala.collection.immutable.Queue

object Functions {

  // Zad 1 (2.5pkt)
  // Jako kolekcję wynikową użyłem Queue, ponieważ ona ma złożoność dodania na koniec
  // 0(1). Argumenty mogą być dowolnymi kolekcjami.
  def duplicate[A](elements: Seq[A], times: Seq[Int]): Queue[A] = {
    val zipped = elements.zip(times)
    zipped.foldLeft(Queue[A]())((queue, pair) => queue ++ Queue.fill(pair._2)(pair._1))
  }

  // Zad 2 (2.5pkt)
  // Użyłem distinctBy, żeby usunąć duplikaty
  def duplicateDistinct[A](elements: Seq[A], times: Seq[Int]): Queue[A] = {
    val zipped = elements.zip(times).distinctBy(pair => pair._1)
    zipped.foldLeft(Queue[A]())((queue, pair) => queue ++ Queue.fill(pair._2)(pair._1))
  }



}
