//zad4 5pkt

def eachNElement[A](list:LazyList[A],n:Int,m:Int):LazyList[A] = {
  if (n < 0 || m < 0) throw new Exception("invalid arguments")
  def eachNElementHelp[A](list:LazyList[A],i:Int,j:Int):LazyList[A] =
    (list,i,j) match {
      case (_,_,0) => LazyList()
      case (LazyList(),_,_) => LazyList()
      case (h #:: t,0,_) => h #:: eachNElementHelp(t,n-1,j-1)
      case (_ #:: t,_,_) => eachNElementHelp(t,i-1,j-1)
    }
  eachNElementHelp(list,0,m)
}

val lfib= {
  def lfibHelp(x: BigInt, y: BigInt): LazyList[BigInt] =
    x #:: lfibHelp(y,x+y)
  lfibHelp(0,1)
}

lfib.take(10).force
eachNElement(lfib,2,10).force
eachNElement(lfib,3,10).force
eachNElement(LazyList(),3,3).force
eachNElement(LazyList(1,2,3,4,5,6),2,3).force