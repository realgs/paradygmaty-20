object LazyLists {

  //Zad 4 (5 pkt)
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] = {
      def checkElement(checkList: LazyList[A], counter: Int): LazyList[A] = {
        if(counter == m) LazyList()
        else checkList match{
            case head #:: tail =>
              if(counter % n == 0) head #:: checkElement(tail, counter + 1)
              else checkElement(tail, counter + 1)
            case _ => LazyList()
          }
      }
    if(n <= 0 || m < 0) throw new Exception("Illegal value of Int argument")
    else checkElement(list, 0)
  }

  //Zad 5 (5pkt)
  def + (x: Double, y: Double): Double = x + y

  def ldzialanie(l1: LazyList[Double], l2: LazyList[Double], operation: (Double, Double) => Double): LazyList[Double] = {
    (l1, l2) match{
      case (LazyList(), LazyList()) => LazyList()
      case (h1 #:: t1, h2 #:: t2) => operation(h1, h2) #:: ldzialanie(t1, t2, operation)
      case (LazyList(), _) => l2
      case (_, LazyList()) => l1
    }
  }

}
