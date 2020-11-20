import scala.util.Random

object DataStructures {

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  //Zad1 (3pkt)
  def generateTree(depth: Int, rand: Random, min: Int, max: Int): BT[Int] = {
    def generate(currDepth: Int): BT[Int] =
      if (currDepth == 0) Empty
      else Node(min + rand.nextInt(max - min + 1), generate(currDepth - 1), generate(currDepth - 1))

    if (min < 0 || max < 0) throw new IllegalArgumentException("Range has to two positive numbers!")
    if (min > max) throw new IllegalArgumentException("Min has to be smaller than max!")

    if (rand == null) throw new IllegalArgumentException("You have to provide a valid Random object!")

    if (depth > 0) generate(depth)
    else if (depth == 0) Empty
    else throw new IllegalArgumentException("Depth has to be a positive number!")
  }

  //Zad2 (3pkt)
  def subtractTrees(first: BT[Int], second: BT[Int]): BT[Int] =
    (first, second) match {
      case (Node(v1, l1, r1), Node(v2, l2, r2)) => Node(v1 - v2, subtractTrees(l1, l2), subtractTrees(r1, r2))
      case (Empty, Empty) => Empty
      case (_, _) => throw new IllegalArgumentException("Trees don't have the same depth or one of them is not full!")
    }

  //Zad3 (4pkt)
  //  def removeDuplicates (first: BT[Int], second: BT[Int]): (BT[Int], BT[Int]) = {

  //  }

  def breadthBT[A](bt: BT[A]): List[A] = {
    @scala.annotation.tailrec
    def breadth(queue: List[BT[A]], accum: List[A]): List[A] =
      queue match {
        case List(Empty) => accum.reverse
        case Empty :: t => breadth(t, accum)
        case Node(v, l, r) :: t => breadth(t ::: List(l, r), v :: accum)
      }

    breadth(List(bt), List())
  }


  //Zad4 (5pkt)
  def eachNElement[A](list: LazyList[A], step: Int, last: Int): LazyList[A] = {

    def construct(list: LazyList[A], index: Int): LazyList[A] = {
      if (index == last) LazyList()
      else if (index % step == 0) list.head #:: construct(list.tail, index + 1)
      else construct(list.tail, index + 1)
    }

    if (step < 1) throw new IllegalArgumentException("Step argument has to be a positive integer!")
    if (last < 0) throw new IllegalArgumentException("Last argument has to be a non-negative integer!")

    construct(list, 0)
  }

  //Zad5 (5pkt)
  def lazyAction(first: LazyList[Int], second: LazyList[Int], action: Char): LazyList[Int] = {
    def calculate(operation: (Int, Int) => Int, first_sub: LazyList[Int], second_sub: LazyList[Int]): LazyList[Int] = {
      (first_sub, second_sub) match {
        case (v1 #:: t1, v2 #:: t2) => operation(v1, v2) #:: calculate(operation, t1, t2)
        case (LazyList(), _) => second_sub
        case (_, LazyList()) => first_sub
      }
    }

    action match {
      case '+' => calculate((fst: Int, snd: Int) => fst + snd, first, second)
      case '-' => calculate((fst: Int, snd: Int) => fst - snd, first, second)
      case '/' => calculate((fst: Int, snd: Int) => fst / snd, first, second)
      case '*' => calculate((fst: Int, snd: Int) => fst * snd, first, second)
      case _ => throw new NotImplementedError("This operator isn't supported!")
    }

  }


}
