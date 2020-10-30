class Zad_4 {

  /* 4) Napisz funkcję wyszukującą po/w elementach listy wejściowej. Przyjmuje dwa argumenty wejściowe - lista,
 w której szukamy wartości. Drugi argument - element, którego szukamy. Zwracamy listę elementów,
 które zawierają w sobie lub są równe szukanej frazie. Dodatkowo, zaprojektuj funkcję tak,
 by przyjmowała N fraz wejściowych i zwracała wyniki, które zawierają którąkolwiek z fraz.*/

  def ++ [A] (lA: List[A], lB: List[A]): List[A] =
    lA match {
      case Nil => lB
      case hd :: tl => hd :: ++(tl, lB)
    }

  def contains (str: String, elem: String): Boolean = {
    @scala.annotation.tailrec
    def helper(str: String, elem: String, started: Boolean): Boolean = {
      if (elem == null || elem == "") true
      else if (str == null || str == "") false
      else if (str.head == elem.head) helper(str.tail, elem.tail, started = true)
      else if (str.head != elem.head && !started) helper(str.tail, elem, started = false)
      else false
    }
    helper(str, elem, started = false)
  }

  def search (list: List[String], element: String): (List[String], List[String]) = {
    (list, element) match {
      case (Nil, _) => (Nil, Nil)
      case (_, null) => (Nil, Nil)
      case (hd :: tl, _) if contains(hd, element) => val (xs1, xs2) = search(tl, element); (hd :: xs1, xs2)
      case (hd :: tl, _) if !contains(hd, element) => val (xs1, xs2) = search(tl, element); (xs1, hd :: xs2)
    }
  }

  def tailRec_searchManyElem(list: List[String], elements: List[String]): List[String] = {
    @scala.annotation.tailrec
    def helper(list: List[String], elem: List[String], acc: List[String]): List[String] = {
      (list, elem) match {
        case (Nil, _) => Nil
        case (_, Nil) => acc
        case (_ :: _, h2::t2) => val (xs1, xs2) = search(list, h2); helper(xs2, t2, ++(acc, xs1))
      }
    }
    helper(list, elements, Nil)
  }

  def searchManyElem(list: List[String], elements: List[String]): List[String] = {
    (list, elements) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_ :: _, h2::t2) => val (xs1, xs2) = search(list, h2); ++(xs1, searchManyElem(xs2, t2))
    }
  }

}
