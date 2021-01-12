object lazyLists {

  //zadanie 4 (5 pkt)
  def EachNElement[A](lazyList: LazyList[A], n: Int, lastIndex: Int): LazyList[A] = {
    def helper(list: LazyList[A], x: Int, m: Int): LazyList[A] = {
      if (m == lastIndex) LazyList()
      else
        (list, x) match {
          case (LazyList(), _) => LazyList()
          case (head #:: tail, 1) => head #:: helper(tail, n, m + 1)
          case (_ #:: tail, _) => helper(tail, x - 1, m + 1)
        }
    }

    if (lazyList.isEmpty) return LazyList()
    else if (n < 1 || lastIndex < 0) throw new IllegalArgumentException("Incorrect argument given")
    else if (lastIndex == 0) return LazyList()
    else lazyList.head #:: helper(lazyList, n + 1, 0)
  }

  //zadanie 5 (5pkt)
  def ldzialanie(lazyList1: LazyList[BigDecimal], lazyList2: LazyList[BigDecimal], function: (BigDecimal, BigDecimal) => BigDecimal): LazyList[BigDecimal] = {
    (lazyList1, lazyList2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (LazyList(), _) => lazyList2
      case (_, LazyList()) => lazyList1
      case (_, _) => function(lazyList1.head, lazyList2.head) #:: ldzialanie(lazyList1.tail, lazyList2.tail, function)
    }
  }

  def +(a: BigDecimal, b: BigDecimal): BigDecimal = a + b

  def -(a: BigDecimal, b: BigDecimal): BigDecimal = a - b

  def *(a: BigDecimal, b: BigDecimal): BigDecimal = a * b

  def /(a: BigDecimal, b: BigDecimal): BigDecimal = if (b == 0.0) throw new Exception("Cannot divide by zero") else a / b

}

