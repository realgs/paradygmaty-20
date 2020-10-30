import scala.annotation.tailrec

object L3 {
  //zadanie 1
  def podziel(list: List[Int]): (List[Int], List[Int]) = {
    def filter[A](list: List[A], pred: A => Boolean): List[A] =
      list match {
        case Nil => Nil
        case head :: tail => if (pred(head)) head :: filter(tail, pred) else filter(tail, pred)
      }

    (filter(list, negative), filter(list, negativeAndOdd))
  }

  private def negative(num: Int): Boolean = {
    num < 0
  }

  private def negativeAndOdd(num: Int): Boolean = {
    num % 2 != 0 && negative(num)
  }

  //zadanie 2
  def dlugosc[A](list: List[A]): Int = {
    @scala.annotation.tailrec
    def recTail[B](recList: List[B], len: Int): Int = {
      recList match {
        case Nil => len
        case _ => recTail(recList.tail, len + 1)
      }
    }

    recTail(list, 0)
  }

  //zadanie 3
  def polacz[A](list1: List[A], list2: List[A]): List[A] = {
    @scala.annotation.tailrec
    def tailRec(list1: List[A], list2: List[A], mergedList: List[A]): List[A] = {
      if (list1 == Nil) {
        if (list2 != Nil) return mergedList ++ list2
        else return mergedList
      }
      else tailRec(list2, list1.tail, mergedList :+ list1.head)
    }

    tailRec(list1, list2, Nil)
  }

  //zadanie 4
  @tailrec
  def matchingPat(text: String, pattern: String): Boolean = {
    @tailrec
    def stringContainsInner(txt: String, pat: String): Boolean =
      (txt, pat) match {
        case (_, "") => return true
        case ("", _) => return false
        case (_, _) => if (txt.head != pat.head) return false else stringContainsInner(txt.tail, pat.tail)
      }

    if (text == "" || pattern == "") return false
    else if (text.head == pattern.head && stringContainsInner(text.tail, pattern.tail)) return true
    else matchingPat(text.tail, pattern)
  }

  @tailrec
  def anyMatch(txt: String, patterns: List[String]): Boolean = {
    patterns match {
      case Nil => false
      case head :: tail => if (matchingPat(txt, head)) true else anyMatch(txt, tail)
    }
  }

  //wersja bez rekursji ogonowej
  def find(txtList: List[String], patterns: List[String]): List[String] = {
    txtList match {
      case Nil => Nil
      case (head :: tail) => if (anyMatch(head, patterns)) head :: find(tail, patterns) else find(tail, patterns)
    }
  }

  //wersja z rekursją ogonową
  def findTail(txtList: List[String], patterns: List[String]): List[String] = {
    @tailrec
    def findTailRec(list: List[String], pat: List[String], results: List[String]): List[String] = {
      list match {
        case Nil => results
        case (head :: tail) => if (anyMatch(head, pat)) findTailRec(tail, pat, results :+ head) else findTailRec(tail, pat, results)
      }
    }

    findTailRec(txtList, patterns, Nil)
  }

  //zadanie 5
  //wersja z rekursją ogonową
  def jointListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @scala.annotation.tailrec
    def tailRec(l1: List[A], l2: List[A], l3: List[A], mergedList: List[A]): List[A] = {
      l1 match {
        case Nil =>
          if (l2 != Nil) tailRec(l2, l3, l1, mergedList)
          else if (l3 != Nil) tailRec(l3, l1, l2, mergedList)
          else mergedList
        case head :: tail => tailRec(tail, l2, l3, mergedList :+ head)
      }
    }

    tailRec(list1, list2, list3, Nil)
  }

  //wersja bez rekursji ogonowej
  def jointLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    list1 match {
      case Nil =>
        if (list2 != Nil) jointLists(list2, list3, list1)
        else if (list3 != Nil) jointLists(list3, list1, list2)
        else Nil
      case head :: tail => head :: jointLists(tail, list2, list3)
    }
  }

}


