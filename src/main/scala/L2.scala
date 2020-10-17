object L2 {

  // Zadanie 1
  def multiplyListElements(list: List[Double]) : Double = {
    if(list == Nil) 0
    else if(list.tail == Nil) list.head
    else list.head * multiplyListElements(list.tail)
  }



}
