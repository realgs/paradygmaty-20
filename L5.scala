import scala.collection.immutable.Queue

object L5 {
  // Zadanie 1 (2.5pkt)
  // Zastosowałem w tym zadaniu kolejkę, ponieważ w moim rozwiązaniu często dodaje na koniec mojej
  // kolekcji element <- ta operacja dla kolejki ma złożoność stałą

  def duplicate[A](collection:Queue[A],repetitions:Queue[Int]):Queue[A]={
    def repeatElement(result:Queue[A],element:A,count:Int):Queue[A]={
      if(count<=0)result
      else repeatElement(result.enqueue(element),element,count-1)
    }
    def repeat(result:Queue[A],collection:Queue[A],repetitions:Queue[Int]):Queue[A]={
      (collection,repetitions) match{
        case (Queue(),Queue()) | (Queue(),_) | (_,Queue())=> result
        case (c,r) => repeat(repeatElement(result,c.head,r.head),c.tail,r.tail)
      }
    }
    repeat(Queue(),collection,repetitions)
  }
}
