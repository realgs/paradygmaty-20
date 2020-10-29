// Kacper Kochański

// Funkcje pomocnicze
def rev[A](lista:List[A]):List[A]={
  def revHelper[A](lista:List[A],revLista:List[A]):List[A]={
    if(lista == Nil) revLista
    else revHelper(lista.tail,lista.head::revLista)
  }
  revHelper(lista,Nil)
}

def mergeList[A](list1:List[A],list2:List[A]):List[A]={
  def mergeListHelper(list1:List[A],list2:List[A],result:List[A]):List[A]={
    (list1,list2) match{
      case (Nil,Nil) => rev(result)
      case (Nil,head::tail) => mergeListHelper(list1,tail,head::result)
      case (head::tail,_) => mergeListHelper(tail,list2,head::result)
    }
  }
  mergeListHelper(list1,list2,Nil)
}

def stringEquals(f:String,s:String):Boolean={
  def stringEqualsHelper(f:String,s:String):Boolean= {
    if (f == "" && s == "") true
    else if (f == "" && s != "") false
    else if (f != "" && s == "") true
    else f.head == s.head && stringEqualsHelper(f.tail, s.tail)
  }

  def equals(f:String,s:String):Boolean={
    if(f == "") false
    else stringEqualsHelper(f,s) || equals(f.tail,s)
  }

  equals(f,s)
}

