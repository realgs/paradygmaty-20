object Zadanie1 extends App{
  def product(list: List[Double]): Double =
    if(list.isEmpty) 0
    else if(list.tail!=Nil) list.head * product(list.tail)
    else list.head

  println(product(Nil)==0)
  println(product(List(1.7, 2, 5.75, 13.75))==268.8125)
  println(product(List(1.27))==1.27)
  println(product(List(-8.8, 1.2, 5, 3.4))==(-179.52))
}
