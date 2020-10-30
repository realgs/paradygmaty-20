

def listLenght[A](list:List[A]):Int = {
  def help(acc:Int,list:List[A]):Int = list match {
    case Nil => acc
    case head::tail => help(acc+1,list.tail)
  }
  help(0,list)
}