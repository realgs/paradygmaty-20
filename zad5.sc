def ldzialanie(list1:LazyList[Int],list2:LazyList[Int],operator:String):LazyList[Int] = {
  operator match {
    case "+" => def add(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
      }
    case "-" => def substract(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: substract(t1, t2)
      }
    case "*" => def multiply(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: multiply(t1, t2)
      }
    case "/" => def divide(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] = {
      (list1, list2) match {
        case (LazyList(), LazyList()) => LazyList()
        case (LazyList(), list2) => list2
        case (list1, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: divide(t1, t2)
      }
    }
    case _ => throw new Exception("Unsupported Operation Exception")
  }
}