
object l5 {
  //W zadaniach 1, 2 użyłem Listy jako reprezentacji kolekcji (wstawiając element zawsze na początek),
  //ponieważ zapewnia to złożoność wstawiania O(1) (plus O(n) -> ewentualne odwrócenie listy ,jak w add_Elements,
  //zamiast wstawiać na koniec, co doprowadziłoby do złozoności rzędy n^2)

  //Zad1 (2.5 pkt)
  //Zł. pes. O(k1+ k2 + k3+...+k_max(n, m)) gdzie k1, k2, k3, to krotności (ile razy powtórzyć) dany element, n <-dł. xs, m <- dł. how_Many_Times
  def replicate[A](xs: List[A])(how_Many_Times: List[Int]): List[A] = {
    how_Many_Times match {
      case List() => List()
      case hd :: _ =>
        if(xs.nonEmpty) {
          if (hd > 0) xs.head :: replicate(xs)((hd - 1) :: how_Many_Times.tail)
          else replicate(xs.tail)(how_Many_Times.tail)
        }else List()
    }
  }

  //Zad2 (2.5 pkt)
  //Zł. pes. O(n*k +2n) => O(n*k)
  def delete_Duplicates[A](xs: List[A]): List[A] = {
    //czy możemy dodać element do listy wynikowej
    //Zł. pes. O(n)
    def check_Element(ys: List[A])(y: A): Boolean = {
      ys match {
        case List() => true
        case _ =>
          if (ys.head == y) false
          else true & check_Element(ys.tail)(y)
      }
    }

    //Zł. pes. O(1/2*n^2+n)
    // O(n*(k) + n), przejście po liście ze sprawdzaniem czy nie ma go już w liście wynikowej (o długości k)
    //  + odwrócenie,
    def add_Elements(ret_List: List[A])(zs: List[A]): List[A] = {
      zs match {
        case List() => ret_List
        case hd :: tl => if (check_Element(ret_List)(hd)) {
          add_Elements(hd :: ret_List)(tl)
        }
        else add_Elements(ret_List)(tl)
      }
    }
    if(xs.nonEmpty) add_Elements(xs.head::List())(xs.tail).reverse
    else List()
  }

  //Zł. pes. O(n*m + k1+ k2 + k3+...) -> O(n*n) gdzie k1, k2, k3, to krotności (ile razy powtórzyć) dany element
  //n <- długość xs, m<- długość xs bez powtórzeń
  def replicate_No_Duplicates[A](xs: List[A])(how_Many_Times: List[Int]): List[A] = {
    replicate(delete_Duplicates(xs))(how_Many_Times)
  }
}

//Zad3 (5pkt)
trait Debug {
  def debug_Name() = {
    println(getClass)
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

//Zad4 (5pkt)
trait Debug2 {
  def debug_Vars() = {
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      println("Var: " + field.getName + " => " + field.getType + ", " + field.get(this))
    }
  }
}

class Point2(xv: Int, yv: Int) extends Debug2 {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

//Zad5 (5pkt)
trait Debug3 {
  def get_Debug_Name: String = getClass.getSimpleName
  def get_Debug_Vars:(List[String], List[Class[_]], List[AnyRef])  = {
    var ret_list_Name = List[String]()
    var ret_list_Type = List[Class[_]]()
    var ret_list_Value = List[AnyRef]()
    for(field <- getClass.getDeclaredFields){
      field.setAccessible(true)
      ret_list_Name = field.getName::ret_list_Name
      ret_list_Type = field.getType::ret_list_Type
      ret_list_Value = field.get(this)::ret_list_Value
    }
    (ret_list_Name.reverse, ret_list_Type.reverse, ret_list_Value.reverse)
  }
}

class Point3(xv: Int, yv: Int) extends Debug3 {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}


