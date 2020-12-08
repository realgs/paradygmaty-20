class Zad_5 {

  // 5) pkt
  def ++ [A] (lA: List[A], lB: List[A]): List[A] =
    lA match {
      case Nil => lB
      case hd :: tl => hd :: ++(tl, lB)
    }

  def tailRec_linkThreeLists [A] (lA: List[A], lB: List[A], lC: List[A]): List[A] = {
    @scala.annotation.tailrec
    def helper( listA: List[A], listB: List[A], listC: List[A], acc: List[A]): List[A] =
      (listA, listB) match {
        case (Nil, Nil) => ++(acc, listC)
        case (Nil, hd :: tl) => helper(Nil, tl, listC, ++(acc, List(hd)))
        case (hd :: tl, _) => helper(tl, listB, listC, ++(acc, List(hd)))
      }
    helper(lA, lB, lC, Nil)
  }

  def linkThreeLists [A] (lA: List[A], lB: List[A], lC: List[A]): List[A] =
    (lA, lB) match {
      case (Nil, Nil) => lC
      case (Nil, hd :: tl) => hd :: linkThreeLists(Nil, tl, lC)
      case (hd :: tl, _) => hd :: linkThreeLists(tl, lB, lC)
    }

}
