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

  // Zadanie 2 (2.5pkt)
  // W tym zadaniu aby zapewnić, że elementy pierwszej kolekcji będą unikalne
  // korzystam z metody kolekcji Queue -> distinct działającej w złożoności liniowej
  // i zwracającej kolejkę bez powtarzających się elementów

  def duplicateDistinct[A](collection:Queue[A],repetitions:Queue[Int]):Queue[A]={
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
    repeat(Queue(),collection.distinct,repetitions)
  }

  // Zadania 3,4,5
  trait Debug{
    // Zadanie 3 (5pkt.)
    def debugName():Unit={val name = this.getClass.getSimpleName;println(s"Class: $name" )}
    // Zadanie 4 (5pkt.)
    def debugVars():Unit={val fields = this.getClass.getDeclaredFields
      fields.foreach(v =>
        if(v.getName != "$outer")
        {
          v.setAccessible(true)
          println("Var: "+v.getName+" => "+v.getType+", "+v.get(this))
        })}

    // Zadanie 5 (5pkt.)
    def getDebugName:String = this.getClass.getSimpleName
    def getDebugVars:Array[(String,Class[_],AnyRef)] = {
      val fields = this.getClass.getDeclaredFields
      var result = Array[(String,Class[_],AnyRef)]()
      fields.foreach(v =>
        if(v.getName != "$outer")
        {
          v.setAccessible(true)
          result = (v.getName,v.getType,v.get(this))+:result
        })
      result.reverse
    }
  }

  // Klasy do testowania
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Circle(ov:Point,rv:Int) extends Debug {
    var o: Point = ov
    var r: Int = rv
  }

  class EmptyClass() extends Debug {

  }
}

