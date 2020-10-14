def iloczyn (list : List[Int]) : Int ={
  if(list == Nil) 0
  else
    if(list.tail == Nil) list.head
    else list.head * iloczyn(list.tail)
}

iloczyn(List(2,1,3,4))
iloczyn(List(-2,1,3,4))
iloczyn(List(2))
iloczyn(List())

def sentence(listOfWord : List[String], sep : String, ter : String) : String ={
  if(listOfWord == Nil) "Empty List"
  else
    if(listOfWord.tail != Nil) listOfWord.head + sep + sentence(listOfWord.tail, sep, ter)
    else listOfWord.head + ter
}

sentence(List("ala", "ma", "kota"), ",", ".")
sentence(List("ala", "ma"), ",", ".")
sentence(List("ala"), ",", ".")
sentence(List(), ",", ".")


def isInInterval(list : List[Int], x : Int, y : Int) : Boolean = {
  if(list == Nil) false
  else
    if(list.tail != Nil) list.head >= x && list.head <= y && isInInterval(list.tail,x,y)
    else list.head >= x && list.head <= y
}

isInInterval(List(3,6,2,4),2,5)
isInInterval(List(2),2,5)
isInInterval(List(1,2),2,5)
isInInterval(List(2,3,4),2,5)
isInInterval(List(),2,5)

def potegi(x: Int, y : Int) : Int = {
    if (y< 0) throw new Exception(s"ujemny argument: $y")
    else if (y == 0) 1
    else x * potegi(x, y - 1)
}
potegi(2,3)
potegi(4,2)
potegi(4,0)
potegi(4,-4)
