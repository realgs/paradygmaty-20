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

  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    @tailrec
    def auxFilter (xs: List[A], accu: List[A]): List[A] = {
      xs match {
        case Nil => reverse(accu)
        case h::t => if (predicate(h)) auxFilter(t, h :: accu) else auxFilter(t, accu)
      }
    }
    auxFilter(xs, List[A]())
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

  // Task 3
  def interlace[A](xs: List[A], ys: List[A]): List[A] = {
    (xs, ys) match {
      case (hx :: tx, hy :: ty) => hx :: hy :: interlace(tx, ty)
      case (Nil, lst) => lst
      case (lst, Nil) => lst
    }
  }

  // Task 4
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

  def add(u: Int, a: Int, c: Char): Int = {
    (u * a) + c.toInt
  }

  def remove(u: Int, removeTerm: Int, c: Char): Int = {
    u - c.toInt * removeTerm
  }

  def rollingHash(s: String, base: Int): Int = {
    s.foldLeft(0)((rh, c) => add(rh, base, c))
  }

  def nextHash(rh: Int, base: Int, removeTerm: Int, first: Char, next: Char): Int = {
    val removed = remove(rh, removeTerm, first)
    add(removed, base, next)
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

  @tailrec
  private def isMatch(pattern: String, offsetS: String): Boolean = {
    (pattern, offsetS) match {
      case ("", _) => true
      case _ => if (pattern.head == offsetS.head) isMatch(pattern.tail, offsetS.tail) else false
    }
  }

  def isSubstring(pattern: String, s: String): Boolean = {
    val SYSTEM_BASE = 256

    val len = strlen(pattern) // O(n)
    val removeTerm = power(SYSTEM_BASE, len - 1)
    val hp = rollingHash(pattern, SYSTEM_BASE)  // O(n)

    // O(m) at most
    @tailrec
    def auxSubstring(s: String, offsetS: String, rh: Int, i: Int): Boolean = {
      if (i >= len) {
        if (rh == hp) {
          if (isMatch(pattern, offsetS)) return true
        }
        (s, offsetS) match {
          case ("", _) => false
          case _ => auxSubstring(s.tail, offsetS.tail, nextHash(rh, SYSTEM_BASE, removeTerm, offsetS.head, s.head), i)
        }
      } else {
        s match {
          case "" => false
          case _ => auxSubstring(s.tail, offsetS, add(rh, SYSTEM_BASE, s.head), i + 1)
        }
      }
    }
    auxSubstring(s, s, 0, 0)
  }

  // Single pattern
  def find(pattern: String, xs: List[String]): List[String] = {
    filter(xs)(x => isSubstring(pattern, x))
  }

  def find(patterns: List[String], xs: List[String]): List[String] = {
    fold_left(patterns, List[String]())((pat, accu) => concatLists(accu, find(pat, xs)))
  }

  def concatLists[A](xs: List[A], ys: List[A]): List[A] = {
    xs match {
      case Nil => ys
      case h :: t => h :: concatLists(t, ys)
    }
  }

  // Task 5
  def joinLists[A](xs: List[A], ys: List[A], zs: List[A]): List[A] = {
    val lst = List(ys, xs)
    fold_left(lst, zs)((xs, accu) => concatLists(xs, accu))
  }
}
