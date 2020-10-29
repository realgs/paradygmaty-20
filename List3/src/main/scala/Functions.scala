import scala.annotation.tailrec

object Functions {
  private val mod = (x: Int, n: Int) => (x % n + n) % n
  private val isOdd = (x: Int) => mod(x, 2) == 1

  @tailrec
  def fold_left[A, B](xs: List[A], accu: B)(f: (A, B) => B): B = {
    xs match {
      case Nil => accu
      case h :: t => fold_left(t, f(h, accu))(f)
    }
  }

  def reverse[A](xs: List[A]): List[A] = {
    fold_left(xs, List[A]())(_ :: _)
  }

  // Task 1
  val split: List[Int] => (List[Int], List[Int]) = xs => {
    val (left, right) = fold_left(xs, (List[Int](), List[Int]()))((x, accu) => {
      if (x < 0) {
        if (isOdd(x)) (x :: accu._1, x :: accu._2)
        else (x :: accu._1, accu._2)
      } else accu
    })
    (reverse(left), reverse(right))
  }

  // Task 2
  def length[A](xs: List[A]): Int = {
    fold_left(xs, 0)((_, sum) => sum + 1)
  }

  /*
  def zip [A](xs: List[A], ys: List[A]): List[List[A]] = {
    (xs, ys) match {
      case (hx::tx, hy::ty) => List(hx, hy) :: zip(tx, ty)
      case (Nil, Nil) => Nil
      case (Nil, lst) => List(lst)
      case (lst, Nil) => List(lst)
    }
  }

  def flatten [A](xs: List[List[A]]): List[A] = {
    fold_left(xs, List[A]())((accu, x) => x ++ accu)
  }

   */

  // Task 3
  def interlace[A](xs: List[A], ys: List[A]): List[A] = {
    (xs, ys) match {
      case (hx :: tx, hy :: ty) => hx :: hy :: interlace(tx, ty)
      case (Nil, lst) => lst
      case (lst, Nil) => lst
    }
  }

  /*
  def add(u: Int, a: Int, prime: Int, c: Char): Int = {
    mod((u * a) + c.toInt, prime)
  }

  def remove(u: Int, a: Int, prime: Int, c: Char): Int = {
    mod(u - c.toInt * mod(math.pow(a, u.abs - 1).toInt, prime), prime)
  }

  def rollingHash(s: String): Int = {
    s.foldLeft(0)((rh, c) => add(rh, 256, 101, c))
  }

  def nextHash(rh: Int, first: Char, next: Char): Int = {
    val removed = remove(rh, 256, 101, first)
    add(removed, 256, 101, next)
  }

  def isSubstring(pattern: String, s: String): Boolean = {
    val x = add(0, 100, 23, 'a')
    print(x)
    print("\n")
    val y = add(x, 100, 23, 'b')
    print(y)
    true
  }

   */

  /*
  val splitOld: List[Int] => (List[Int], List[Int]) = xs => {
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
  */
}
