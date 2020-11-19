class LazyLists {

/*4) Zdefiniuj funkcję "eachNElement" wybierającą co n-ty element listy leniwej,
  zaczynając od elementu pierwszego kończąc na elemencie numer m(indeks m dotyczy listy pierwotnej, nie wynikowej).
  Punkty: 5*/

def eachNElement [A](lxs: LazyList[A], k: Int, toN: Int): LazyList[A] = {
  if(k < 1) throw new IllegalArgumentException
  else {
    def helper(count: Int, n: Int, llist: LazyList[A]): LazyList[A] =
      (count, n, llist) match {
        case (_, _, LazyList()) => LazyList()
        case (1, 1, hd #:: _) => LazyList(hd)
        case (_, 1, _ #:: _) => LazyList()
        case (1, _, hd #:: tl) => hd #:: helper(k, n - 1, tl)
        case (_, _, _ #:: tl) => helper(count - 1, n - 1, tl)
      }
    helper(1, toN, lxs)
  }
}

  /*5) Zdefiniuj funkcję "ldzialanie" przyjmującą dwie listy leniwe i wykonującą podane działanie na elementach list.
  Obsłuż 4 podstawowe operacje matematyczne. Wynikiem jest lista leniwa. Proszę o testy(!)
  Punkty: 5*/

  def add[A : Integral](a1: A, a2: A): A = implicitly[Integral[A]].plus(a1, a2)
  def subtract[A : Integral](a1: A, a2: A): A = implicitly[Integral[A]].minus(a1, a2)
  def multiply[A : Integral](a1: A, a2: A): A = implicitly[Integral[A]].times(a1, a2)
  def divide[I: Integral](a: I, b: I): I = implicitly[Integral[I]].quot(a,b)

  def doOperation [T: Integral](la: LazyList[T], lb: LazyList[T], symbol: String): LazyList[T] = {
    if(!symbol.equals("add") && !symbol.equals("subtract") &&  !symbol.equals("multiply") && !symbol.equals("divide")) throw new IllegalArgumentException
    else {
      def helper[B: Integral](listA: LazyList[B], listB: LazyList[B]): LazyList[B] =
        (listA, listB) match {
          case (_, LazyList()) => LazyList()
          case (LazyList(), _) => LazyList()
          case (_, _) if symbol.equals("add") =>
            add(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("subtract") =>
            subtract(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("multiply") =>
            multiply(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("divide") =>
            divide(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
        }
      helper(la, lb)
    }
  }

  def addF[A : Fractional](a1: A, a2: A): A = implicitly[Fractional[A]].plus(a1, a2)
  def subtractF[A : Fractional](a1: A, a2: A): A = implicitly[Fractional[A]].minus(a1, a2)
  def multiplyF[A : Fractional](a1: A, a2: A): A = implicitly[Fractional[A]].times(a1, a2)
  def divideF[I: Fractional](a: I, b: I): I = implicitly[Fractional[I]].div(a,b)

  def doOperationF [T: Fractional](la: LazyList[T], lb: LazyList[T], symbol: String): LazyList[T] = {
    if(!symbol.equals("add") && !symbol.equals("subtract") &&  !symbol.equals("multiply") && !symbol.equals("divide")) throw new IllegalArgumentException
    else {
      def helper[B: Fractional](listA: LazyList[B], listB: LazyList[B]): LazyList[B] =
        (listA, listB) match {
          case (_, LazyList()) => LazyList()
          case (LazyList(), _) => LazyList()
          case (_, _) if symbol.equals("add") =>
            addF(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("subtract") =>
            subtractF(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("multiply") =>
            multiplyF(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
          case (_, _) if symbol.equals("divide") =>
            divideF(listA.head, listB.head) #:: helper(listA.tail, listB.tail)
        }
      helper(la, lb)
    }
  }

}
