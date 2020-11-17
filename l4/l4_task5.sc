sealed trait nlist[A]
case class Koniec[A]() extends nlist[A]
case class Element[A](elem: A, list: nlist[A])extends nlist[A]

sealed trait llist[A]
case class LKoniec[A]() extends llist[A]
case class LElement[A](elem: A, tail: ()=>llist[A])extends llist[A]

def toNList[A](lxs: llist[A]): nlist[A] =
  lxs match{
    case LKoniec() => Koniec()
    case LElement(hd, tail) => Element(hd, toNList(tail()))
  }

//Zadanie5 (5pkt)
def loperationHelper[A](operation: A=>A=>A)(lxs: llist[A])(lys: llist[A]): llist[A] =
  (lxs, lys) match{
    case (LKoniec(), LKoniec()) => LKoniec()
    case (LKoniec(), LElement( _ , _ )) => throw new Exception("Listy nie są równej długości")
    case (LElement( _ , _ ), LKoniec()) => throw new Exception("Listy nie są równej długości")
    case (LElement(xhd, xtail), LElement(yhd, ytail))
    => LElement(operation(xhd)(yhd), ()=>loperationHelper(operation)(xtail())(ytail()))
  }


def loperation[A](operator: Char)(lxs: llist[Int])(lys: llist[Int]): llist[Int] =
  operator match{
    case '+' =>loperationHelper((x: Int) => (y: Int) => x+y)(lxs)(lys)
    case '-' =>loperationHelper((x: Int) => (y: Int) => x-y)(lxs)(lys)
    case '*' =>loperationHelper((x: Int) => (y: Int) => x*y)(lxs)(lys)
    case '/' =>loperationHelper((x: Int) => (y: Int) => x/y)(lxs)(lys)
    case _ => throw new Exception ("Niewłaściwy operator")
  }

toNList(loperation('+') (LElement(5, ()=>LElement(6, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))))(LElement(5, ()=>LElement(6,
  ()=>LElement(3, ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))))) ==
  Element(10,Element(12,Element(6,Element(4,Element(2,Koniec())))))

toNList(loperation('-') (LElement(5, ()=>LElement(20, ()=>LElement(9,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))))(LElement(6, ()=>LElement(6,
  ()=>LElement(3, ()=>LElement(2, ()=>LElement(8,()=>LKoniec[Int]()))))))) ==
  Element(-1,Element(14,Element(6,Element(0,Element(-7,Koniec())))))

toNList(loperation('*') (LElement(5, ()=>LElement(6, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))))(LElement(11, ()=>LElement(6,
  ()=>LElement(0, ()=>LElement(2, ()=>LElement(1,()=>LKoniec[Int]()))))))) ==
  Element(55,Element(36,Element(0,Element(4,Element(1,Koniec())))))

toNList(loperation('/') (LElement(100, ()=>LElement(700, ()=>LElement(3,
  ()=>LElement(2, ()=>LElement(49,()=>LKoniec[Int]()))))))(LElement(10, ()=>LElement(9,
  ()=>LElement(36, ()=>LElement(-1, ()=>LElement(-7,()=>LKoniec[Int]()))))))) ==
  Element(10,Element(77,Element(0,Element(-2,Element(-7,Koniec())))))

def loperationD[A](operator: Char)(lxs: llist[Double])(lys: llist[Double]): llist[Double] =
  operator match{
    case '+' =>loperationHelper((x: Double) => (y: Double) => x+y)(lxs)(lys)
    case '-' =>loperationHelper((x: Double) => (y: Double) => x-y)(lxs)(lys)
    case '*' =>loperationHelper((x: Double) => (y: Double) => x*y)(lxs)(lys)
    case '/' =>loperationHelper((x: Double) => (y: Double) => x/y)(lxs)(lys)
    case _ => throw new Exception ("Niewłaściwy operator")
  }

toNList(loperationD('+') (LElement(5.0, ()=>LElement(6.7, ()=>LElement(3.9,
  ()=>LElement(2.7, ()=>LElement(1.6,()=>LKoniec[Double]()))))))(LElement(8.0, ()=>LElement(6.30,
  ()=>LElement(-3.9, ()=>LElement(2.2, ()=>LElement(1.2,()=>LKoniec[Double]()))))))) ==
  Element(13.0,Element(13.0,Element(0.0,Element(4.9,Element(2.8,Koniec())))))

