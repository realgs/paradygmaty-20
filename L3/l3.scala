//2
/*
zlozonosc:
pamieciowa:O(n), n-długosc podanej listy
obliczeniowa:O(1),
*/
def listLenght[A](list:List[A]):Int = {
  def help(acc:Int,list:List[A]):Int = list match {
    case Nil => acc
    case head::tail => help(acc+1,list.tail)
  }
  help(0,list)
}
//2 test
listLenght(List())
listLenght(List(1))
listLenght(List(1,2))
listLenght(List('a','b','c'))