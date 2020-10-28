object Functions {
  import scala.annotation.tailrec

  val isEven = (x: Int) => x % 2 == 1

  val split: List[Int] => (List[Int], List[Int])= xs => {
    @tailrec
    def auxSplit(xs: List[Int], left: List[Int], right: List[Int]): (List[Int], List[Int]) = {
      xs match {
        case Nil => (left.reverse, right.reverse)
        case h :: t => {
          if (h < 0) {
            if (h % 2 == -1) {
              auxSplit(t, h :: left, h :: right)
            } else {
              auxSplit(t, h :: left, right)
            }
          } else {
            auxSplit(t, left, right)
          }
        }
      }
    }
    auxSplit(xs, List(), List())
  }


}
