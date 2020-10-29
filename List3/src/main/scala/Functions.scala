import scala.annotation.tailrec

object Functions {
  private val mod = (x: Int, n: Int) => (x % n + n) % n
  private val isOdd = (x: Int) => mod(x, 2) == 1

  def fold_left [A, B](xs: List[A], accu: B)(f: (A, B) => B): B = {
    xs match {
      case Nil => accu
      case h::t => fold_left(t, f(h, accu)) (f)
    }
  }

  def reverse [A](xs: List[A]): List[A] = {
    fold_left(xs, List[A]())(_ :: _)
  }

  val split: List[Int] => (List[Int], List[Int])= xs => {
    @tailrec
    def auxSplit(xs: List[Int], left: List[Int], right: List[Int]): (List[Int], List[Int]) = {
      xs match {
        case Nil => (reverse(left), reverse(right))
        case h :: t => {
          if (h < 0) {
            if (isOdd(h)) {
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
