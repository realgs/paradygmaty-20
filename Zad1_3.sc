
sealed abstract trait T[+A]
case object Empty extends T[Nothing]

case class Node[+A](elem:Int, left:T[A], right:T[A]) extends T[A]


//Zad1 (3pkt)
def generateT[A](depth:Int):T[AnyVal] = {
  val r = util.Random
  if(depth >= 0){
    if (depth == 0) Node(r.nextInt(10), Empty, Empty)
    else Node(r.nextInt(10), generateT(depth - 1), generateT(depth - 1))
  }
  else Empty
}

def depthT[A](tree: T[A]): Int = {
  def count[A](tree: T[A],depth: Int): Int = {
    tree match {
      case Node(elem, left, right) => count(left, depth + 1)
      case Empty => depth - 1
    }
  }

  count(tree,0)

}

def elemCount[A](tree:T[A]):Int={
  var count = 0;
  def counter[A](tree: T[A]): Int = {
    tree match {
      case Node(elem, left, right) =>
        count += 1
        counter(left)
        counter(right)
      case Empty => count + 1
    }
  }
  counter(tree)
  count
}

def full_tree(depth:Int, count:Int):Boolean={
  var counter = scala.math.pow(2,depth+1)-1
  if(counter == count)
    true

  else
    false
}

//Zad2 (3pkt)
def differense[A](tree_1:T[A],tree_2:T[A]): T[AnyVal] ={
  def dif(num_1:Int,num_2:Int):Int = {
    num_1 - num_2
  }

  def new_tree[A](tree_1:T[A],tree_2:T[A]): T[AnyVal] =
  {
    val pair = (tree_1:T[A],tree_2:T[A])
    pair match {
      case (Node(elem, left, right), Node(elem1, left1, right1)) =>
        Node(dif(elem, elem1), new_tree(left, left1), new_tree(right, right1))
      case (Empty, Empty) => Empty
      case (Node(elem, left, right), Empty) => Empty
      case (Empty, Node(elem, left, right)) => Empty
    }
  }
  if((depthT(tree_1) == depthT(tree_2)) && depthT(tree_1) > 0){
    new_tree(tree_1:T[A],tree_2:T[A])
  }
  else
    Empty
}

//Zad3-wgłąb (1pkt)
def ro_depth[A](tree_1:T[A],tree_2:T[A]): (T[AnyVal],T[AnyVal]) ={

  def check(tree_1:T[A],tree_2:T[A]):Boolean={

    val pair = (tree_1:T[A],tree_2:T[A])
    pair match {
      case (Node(elem, left, right), Node(elem1, left1, right1)) =>
        if(elem == elem1) {
          val check_left = check(left, left1)
          val check_right = check(right, right1)
          if (check_left && check_right) {
            true
          }
          else {
            false
          }
        }
        else{
          false
        }
      case (Empty, Empty) => true
      case (Node(elem, left, right), Empty) => false
      case (Empty, Node(elem, left, right)) => false
    }
  }


  if((depthT(tree_1) == depthT(tree_2)) && depthT(tree_1) > 0){
    val pair = (tree_1:T[A],tree_2:T[A])
    pair match {
      case (Node(elem, left, right), Node(elem1, left1, right1)) =>

        if(elem == elem1){
          val check_left = check(left,left1)
          val check_right = check(right,right1)

          if(check_left && check_right) {
            (Empty, Empty)
          }
          else{
            val (sub_left, sub_left_1)  = ro_depth(left,left1)
            val (sub_right, sub_right_1) = ro_depth(right,right1)
            (Node(-1,sub_left,sub_right),Node(-1,sub_left_1,sub_right_1))

          }
        }
        else{
          val (sub_left, sub_left_1)  = ro_depth(left,left1)
          val (sub_right, sub_right_1) = ro_depth(right,right1)
          (Node(elem,sub_left,sub_right),Node(elem1,sub_left_1,sub_right_1))
        }
      case (Empty, Empty) => (Empty,Empty)
      case (Node(elem, left, right), Empty) => (Empty,Empty)
      case (Empty, Node(elem, left, right)) => (Empty,Empty)

    }
  }
  else {
    (Empty ,Empty)
  }

}


val t = Node(3,Node(6,Node(3,Empty,Empty),Node(9,Empty,Empty)),Node(5,Node(0,Empty,Empty),Node(8,Empty,Empty)))

val t1 = generateT(2)
full_tree(depthT(t1),elemCount(t1))
val t2 = generateT(2)
full_tree(depthT(t2),elemCount(t2))

val t3= generateT(10)
depthT(t3)
elemCount(t3)
full_tree(depthT(t3),elemCount(t3))

ro_depth(t1,t2)
differense(t1,t2)
