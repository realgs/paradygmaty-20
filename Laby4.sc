sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]//definicja drzew z wykladu

def breadthBT[A](tree: BT[A]): List[A] = {
  def bthelp[A](nodes: List[BT[A]]):List[A] = {
    nodes match {
      case Nil => Nil
      case Empty :: tail => bthelp(tail)
      case Node(value, left, right) :: tail => value :: bthelp(tail ++ List(left, right))
    }
  }
  bthelp(List(tree))
}//przejscie po elementach drzewa

//zad 1 3 pkt
def generateTree(height: Int, min: Int, max: Int):BT[Int]= {
  val r = scala.util.Random
  if(height<=0)
    return Empty
  def genHelp(currentHeight: Int):BT[Int] = {
    if(currentHeight+1==height)
      return Node(r.nextInt(max-min)+min,Empty,Empty)
    else
      return Node(r.nextInt(max-min)+min,genHelp(currentHeight+1),genHelp(currentHeight+1))
  }
  genHelp(0)
}
val t = generateTree(3,0,5)
val t2 = generateTree(6,100,200)
val t3 = generateTree(0,1,1)
breadthBT(t)
breadthBT(t2)
breadthBT(t3)

//zad2 3 pkt

def subTree(tree: BT[Int], tree2: BT[Int]):BT[Int] = {
  (tree,tree2) match{
    case (Empty,_) => Empty
    case (_,Empty) => Empty
    case (Node(elem,left,right),Node(elem2,left2,right2)) => Node(elem-elem2,subTree(left,left2),subTree(right,right2))
  }
}
val t4 = generateTree(3,0,5)
val t5 = generateTree(3,0,5)
val t6 = subTree(t4,t5)
breadthBT(t4)
breadthBT(t5)
breadthBT(t6)

//zad 4 5 pkt

def eachNElement(list: LazyList[Int], n:Int, m:Int) : LazyList[Int] = {
  if(m>0)
    return list.head#::eachNElement(list.drop(n),n,m-n)
  else return LazyList()
}
val l=LazyList(5,6,3,2,1)
eachNElement(l,2,3).force
eachNElement(l,2,4).force

//zad 5 5 pkt

def ldzialanie(list1: LazyList[Int], list2: LazyList[Int], op: Char) : LazyList[Int] = {
  if(list1==LazyList() && list2==LazyList()) {
    return LazyList()
  }else {
    if (list1 == LazyList()) {
      val val1 = 0
      val val2 = list2.head
      if (val2 == 0 && op == '/')
        return 0 #:: ldzialanie(LazyList(), list2.tail, '/')
      op match {
        case '+' => return (val1 + val2) #:: ldzialanie(LazyList(), list2.tail, '+')
        case '-' => return (val1 - val2) #:: ldzialanie(LazyList(), list2.tail, '-')
        case '*' => return (val1 * val2) #:: ldzialanie(LazyList(), list2.tail, '*')
        case '/' => return (val1 / val2) #:: ldzialanie(LazyList(), list2.tail, '/')
      }
    }
    else if (list2 == LazyList()) {
      val val1 = list1.head
      val val2 = 0;
      op match {
        case '+' => return (val1 + val2) #:: ldzialanie(list1.tail, LazyList(), '+')
        case '-' => return (val1 - val2) #:: ldzialanie(list1.tail, LazyList(), '-')
        case '*' => return (val1 * val2) #:: ldzialanie(list1.tail, LazyList(), '*')
        case '/' => return 0 #:: ldzialanie(list1.tail, LazyList(), '/')
      }
    }
    else {
      val val1 = list1.head
      val val2 = list2.head
      if (val2 == 0 && op == '/')
        return 0 #:: ldzialanie(LazyList(), list2.tail, '/')
      op match {
        case '+' => return (val1 + val2) #:: ldzialanie(list1.tail, list2.tail, '+')
        case '-' => return (val1 - val2) #:: ldzialanie(list1.tail, list2.tail, '-')
        case '*' => return (val1 * val2) #:: ldzialanie(list1.tail, list2.tail, '*')
        case '/' => return (val1 / val2) #:: ldzialanie(list1.tail, list2.tail, '/')
      }

    }
  }
}
val l2=LazyList(1,2,3)
val l3=LazyList(2,3,4,5)
ldzialanie(l3,l2,'/').force
