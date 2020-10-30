import scala.annotation.tailrec

object Laboratorium3 extends App {

  def reversed[T](array:List[T]):List[T]={

    def rev[T](arr:List[T],reverseList:List[T]):List[T]={
      arr match{
        case Nil => reverseList;
        case hd::tl => rev(tl,hd::reverseList);
      }
    }
    rev(array,Nil);
  }

  val forReverse:List[Int] = List(5,4,3,2,1);
  println("Revrese = " + reversed(forReverse));

  //Zadania 2
  def length[T](list: List[T]):Int={
    if(list == Nil) return 0 else 1 + length(list.tail);
  }

  val listEmpty = (Nil);
  val listNumber:List[Int] = List[Int](1,4,-6);
  val listString:List[String] = List[String]("Ala","wczoraj","miła","kota");
  val listDouble:List[Double] = List[Double](4100.4,14.0,97.3,145.6,4.66);

  println("Zadania 2");
  println(length(listEmpty));
  println(length(listNumber));
  println(length(listString));
  println(length(listDouble));

  //Zadania 3
  def join[T](arrayA:List[T],arrayB:List[T]):List[T]={
    (arrayA, arrayB) match{
        case(Nil,Nil)=> Nil;
        case(Nil,hd::tl) => hd::join(Nil,tl);
        case(hd::tl,Nil) => hd::join(tl,Nil);
        case(hd1::tl1, hd2::tl2) => hd1::hd2::join(tl1,tl2);
    }
  }

  val listNumberEven:List[Int] = List[Int](2,4,6);
  val listNumberOdd:List[Int] = List[Int](1,3,5);
  println("Zadania 3");
  println(join(listNumberOdd,listNumberEven));
  println(join(listNumberOdd,listEmpty));
  println(join(listEmpty,listNumberEven));
  val listStringJoin:List[String] = List[String]("czy","nie","ma");
  println(join(listString,listStringJoin));

  println("Zadania 5");

  def joinLists[T](one:List[T],two:List[T],three:List[T]):List[T]={
    (one, two, three) match{
      case(Nil,Nil,Nil)=> Nil;
      case(hd::tl,_,_) => hd::joinLists(tl,two,three);
      case(_,hd::tl,_) => hd::joinLists(one,tl,three);
      case(_,_,hd::tl) => hd::joinLists(one,two,tl);
    }
  }

  val listOne:List[Int] = List[Int](1,2,3);
  val listTwo:List[Int] = List[Int](4,5);
  val listThree:List[Int] = List[Int](6,7,8);

  println(joinLists(listOne,listTwo,listThree));
  println(joinLists(listOne,listEmpty,listThree));
  println(joinLists(listEmpty,listEmpty,listThree));

  println(joinLists(listStringJoin,listString,listStringJoin));

}
