import scala.annotation.tailrec

//TASK 1

def divideListWithNegativeElements(list:List[Int]):(List[Int],List[Int]) = {
  def getNegativeList(x:List[Int]):List[Int]=
    if(x==List())
      List()
    else if(x.head<0)
      x.head :: getNegativeList(x.tail)
    else
      getNegativeList(x.tail)

  def getOddList(y:List[Int]):List[Int] =
    if(y==List())
      List()
    else if(y.head<0 && y.head%2!=0)
      y.head :: getOddList(y.tail)
    else
      getOddList(y.tail)
  (getNegativeList(list),getOddList(list))
}

divideListWithNegativeElements(List(-3,-6,8,-9,13))
divideListWithNegativeElements(List(2,3,8,1,13))
divideListWithNegativeElements(List(-3,-2,-11))
divideListWithNegativeElements(List())
divideListWithNegativeElements(List(2))
divideListWithNegativeElements(List(-5))
divideListWithNegativeElements(List(-6))

//TASK 2

def listLength[A](list:List[A]):Int =
  {
    @tailrec
    def iter[B](x:List[B],len:Int): Int =
      if(x==List())
        len
      else
        iter(x.tail,len+1)

    iter(list,0)
  }
listLength(List())
listLength(List(1,2,3,4))
listLength(List(List('a','b'),1,4))

//zlozonosc obliczeniowa - dlugosc listy / zlozonosc pamieciowa - ramka funkci listLength

//TASK 3

def concatenateListsAlternately[A](left:List[A],right:List[A]):List[A] =
  (left,right) match
  {
    case (Nil,Nil) => Nil
    case  (Nil,r) => r
    case  (l,Nil) =>  l
    case  (lhead::ltail,rhead::rtail) => lhead::rhead::concatenateListsAlternately(left.tail,right.tail)
  }

concatenateListsAlternately(List(5,4,3,2),List(1,2,3,4,5,6))
concatenateListsAlternately(List(1,2,3,4,5,6),List(5,4,3,2))
concatenateListsAlternately(Nil,List(1,2,3))
concatenateListsAlternately(List(1,2,3),Nil)
concatenateListsAlternately(List(1,3,5),List(2,4,6))

//zlozonosc pamieciowa - wszystkie wywolania funkcji / zlozonosc obliczeniowa - suma dlugosci obu list

//TASK 4

def findInList(list:List[String],element:String):List[String] =
  (list,element) match{
    case(head::tail,_)=>
      if(head==element)
        head::findInList(list.tail,element)
      else
        findInList(list.tail,element)
    case(Nil,_) => Nil

  }
findInList(List("aaa","ala","bbb","bo"),"ala")
findInList(Nil,"ala")
findInList(List("ala","aaa","bbb","bo"),"a")

def findInListRecTail(list:List[String],element:String):List[String] =
{
  @tailrec
  def iter(l:List[String],el:String,sum:List[String]):List[String] =
    if(l==Nil)
      sum
    else if(l.head==el)
      iter(l.tail,el,l.head::sum)
    else
      iter(l.tail,el,sum)
  iter(list,element,Nil)
}
findInListRecTail(List("aaa","ala","bbb","bo"),"ala")
findInListRecTail(Nil,"ala")
findInListRecTail(List("ala","aaa","bbb","bo"),"")

//TASK 5

def join3Lists[A](list1:List[A],list2:List[A],list3:List[A]):List[A] =
  if(list1!=Nil)
    list1.head :: join3Lists(list1.tail,list2,list3)
  else if(list2!=Nil)
    list2.head :: join3Lists(list1,list2.tail,list3)
  else if(list3!=Nil)
    list3.head :: join3Lists(list1,list2,list3.tail)
  else
    Nil
join3Lists(List(1,2),List(8,7),List(4,5))
join3Lists(List(),List(),List())
join3Lists(List(1,2),List(),List())
join3Lists(List(),List(1,2),List())
join3Lists(List(),List(),List(1,2))
join3Lists(List(1,2),List(),List(5,3))
join3Lists(List(1,2),List(5,4),List())
join3Lists(List(),List(1,2),List(5,3))

// ogonowa

def reverseList[A](l:List[A]):List[A] = {
  var list:List[A] =Nil
  for(el <-l)
    list=el::list
  list
}

def join3ListsRec[A](list1:List[A],list2:List[A],list3:List[A]):List[A] =
{
  @tailrec
  def iter[B](l1:List[B],l2:List[B],l3:List[B],sum:List[B]):List[B] =
    if(l1==Nil && l2==Nil && l3==Nil)
      sum
    else if(l1!=Nil)
      iter(l1.tail,l2,l3,l1.head::sum)
    else if(l2!=Nil)
      iter(l1,l2.tail,l3,l2.head::sum)
    else
      iter(l1,l2,l3.tail,l3.head::sum)
  reverseList(iter(list1,list2,list3,Nil))
}

join3ListsRec(List(1,2),List(8,7),List(4,5))
join3ListsRec(List(),List(),List())
join3ListsRec(List(1,2),List(),List())
join3ListsRec(List(),List(1,2),List())
join3ListsRec(List(),List(),List(1,2))
join3ListsRec(List(1,2),List(),List(5,3))
join3ListsRec(List(1,2),List(5,4),List())
join3ListsRec(List(),List(1,2),List(5,3))

