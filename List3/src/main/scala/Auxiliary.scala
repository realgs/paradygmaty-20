import scala.annotation.tailrec

object Auxiliary {
  private val mod = (x: Int, n: Int) => (x % n + n) % n
  val isOdd: Int => Boolean = (x: Int) => mod(x, 2) == 1

  def power(base: Int, exponent: Int): Int = {
    @tailrec
    def auxPower(exponent: Int, result: Int, power: Int): Int = {
      if (exponent <= 0) result
      else {
        if (exponent % 2 == 1) auxPower(exponent >> 1, result * power, power * power)
        else auxPower(exponent >> 1, result, power * power)
      }
    }

    if (exponent == 0 && base == 0) throw new IllegalArgumentException("0 ** 0 is undefined")

    if (exponent >= 0) {
      auxPower(exponent, 1, base)
    } else {
      auxPower(-exponent, 1, 1 / base)
    }
  }

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

  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    @tailrec
    def auxFilter(xs: List[A], accu: List[A]): List[A] = {
      xs match {
        case Nil => reverse(accu)
        case h :: t => if (predicate(h)) auxFilter(t, h :: accu) else auxFilter(t, accu)
      }
    }

    auxFilter(xs, List[A]())
  }

  def contains[A](xs: List[A], x: A): Boolean = {
    fold_left(xs, false)((element, accu) => (element == x) || accu)
  }

  def distinctElements[A](xs: List[A]): List[A] = {
    reverse(fold_left(xs, List[A]())((x, accu) => if (contains(accu, x)) accu else x :: accu))
  }

  @tailrec
  def fold_left_str[B](s: String, accu: B)(f: (Char, B) => B): B = {
    s match {
      case "" => accu
      case _ => fold_left_str(s.tail, f(s.head, accu))(f)
    }
  }

  val strlen: String => Int = (s: String) => {
    fold_left_str(s, 0)((_, sum) => sum + 1)
  }

  def concatLists[A](xs: List[A], ys: List[A]): List[A] = {
    xs match {
      case Nil => ys
      case h :: t => h :: concatLists(t, ys)
    }
  }

  @tailrec
  def anyMatchArguments[A, B](x: A, arguments: List[B])(predicate: (B, A) => Boolean): Boolean = {
    arguments match {
      case Nil => false
      case h :: t => if (predicate(h, x)) true else anyMatchArguments(x, t)(predicate)
    }
  }
}
