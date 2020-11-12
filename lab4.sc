import scala.util.Random

//zadanie 1 (3pkt)
sealed trait BT[+A]
case class Empty() extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

def generateTree(n: Int,lB: Int, rB: Int): BT[Int]={
    val r=new Random()
    def genInner(n: Int): BT[Int]=
        n match {
          case 0 =>Empty()
          case _ => Node(r.nextInt(rB-lB+1) + lB, genInner(n-1),genInner(n-1))
      }
    genInner(n)
}

def isTreeFull(tree: BT[Int]): Boolean={
    def treeHeight(tree: BT[Int]): Int={
        tree match {
          case Empty()=>0
          case Node(_,l,r)=>{
            val h=treeHeight(l)
            if(h==treeHeight(r))1+h
            else 1
          }
        }
    }
    tree match {
      case Empty()=>true
      case Node(_, l, r)=>treeHeight(l)==treeHeight(r)
    }
}

isTreeFull(generateTree(2,1,1))
isTreeFull(generateTree(2,1,3))
isTreeFull(generateTree(6,1,1))
generateTree(0,1,1)==Empty()

// Zadanie 2 (3pkt)
def subtractionTree(xtree: BT[Int],ytree: BT[Int]): BT[Int]=
  (xtree,ytree) match {
    case (Empty(),_)=>Empty()
    case (_, Empty())=>Empty()
    case (Node(x, xtl, xtr),Node(y, ytl, ytr))=>Node(x - y, subtractionTree(xtl, ytl), subtractionTree(xtr, ytr))
  }

val pre1=Node(2,Node(3,Empty(),Empty()),Node(4,Empty(),Empty()))
val pre2=Node(1,Node(2,Empty(),Empty()),Node(1,Empty(),Empty()))

subtractionTree(pre1,pre2)==Node(1,Node(1,Empty(),Empty()),Node(3,Empty(),Empty()))
isTreeFull(subtractionTree(generateTree(2,1,3),generateTree(2,1,3)))
subtractionTree(generateTree(4,9,9),generateTree(4,6,6))==generateTree(4,3,3)

// Zadanie 3 (4pkt)
def checkTree(xtree: BT[Int],ytree: BT[Int]): Boolean=
  (xtree,ytree)match {
    case (Empty(),Empty())=>true
    case (Node(x,xl,xr),Node(y,yl,yr))=> (x == y) && checkTree(xl,yl)&&checkTree(xr,yr)
  }
def removeDuplicate(check: (BT[Int],BT[Int])=>Boolean,xtree: BT[Int],ytree: BT[Int]): (BT[Int],BT[Int])={
  val Node(x, xtl, xtr) = xtree
  val Node(y, ytl, ytr) = ytree
  val (a,b)=if(x==y)(-1,-1)else(x,y)
  (x==y,check(xtl,ytl),check(xtr,ytr))match {
    case (true,true,true)=>(Empty(),Empty())
    case (_,false,true)=>{
      val (l,r)=removeDuplicate(check,xtl,ytl)
      (Node(a,l,Empty()),Node(b,r,Empty()))
    }
    case (_,true,false)=>{
      val (l,r)=removeDuplicate(check,xtr,ytr)
      (Node(a,Empty(),l),Node(b,Empty(),r))
    }
    case (_,false,false)=>{
      val (ll,lr)=removeDuplicate(check,xtl,ytl)
      val (rl,rr)=removeDuplicate(check,xtr,ytr)
      (Node(a,ll,rl),Node(b,lr,rr))
    }
    case (false,true,true)=>(Node(x,Empty(),Empty()),Node(y,Empty(),Empty()))
  }
}
def removeDuplicateDepth(xtree: BT[Int],ytree: BT[Int]): (BT[Int],BT[Int])=
  removeDuplicate(checkTree,xtree,ytree)


val t1: BT[Int] = Node(1,Node(2,Node(1,Empty(),Empty()),Node(3,Empty(),Empty())),Node(1,Node(2,Empty(),Empty()),Node(1,Empty(),Empty())))
val t2: BT[Int] = Node(2,Node(1,Node(3,Empty(),Empty()),Node(3,Empty(),Empty())),Node(1,Node(1,Empty(),Empty()),Node(1,Empty(),Empty())))
val full4tree= generateTree(4,4,4)
val generatedTree=generateTree(4,5,8)

removeDuplicateDepth(t1,t2)==(Node(1,Node(2,Node(1,Empty(),Empty()),Empty()),Node(-1,Node(2,Empty(),Empty()),Empty())),Node(2,Node(1,Node(3,Empty(),Empty()),Empty()),Node(-1,Node(1,Empty(),Empty()),Empty())))
removeDuplicateDepth(t1,t1)==(Empty(),Empty())
removeDuplicateDepth(full4tree,generatedTree)==(full4tree,generatedTree)

def checkTreeB(xtree: BT[Int],ytree: BT[Int]): Boolean= {
  def checkTreeBQue(queue: List[(BT[Int],BT[Int])]): Boolean =
    if (queue == Nil) true
    else {
      val (l, r) = queue.head
      if(l==Empty()||r==Empty)true
      else
      {
        val Node(x, xl, xr) = l
        val Node(y, yl, yr) = r
        (x == y) && checkTreeBQue(queue.tail ::: List((xl, yl), (xr, yr)))
      }
    }
  checkTreeBQue(List((xtree,ytree)))
}
def removeDuplicateBreadth(xtree: BT[Int],ytree: BT[Int]): (BT[Int],BT[Int])=
    removeDuplicate(checkTreeB,xtree,ytree)

removeDuplicateBreadth(t1,t2)==(Node(1,Node(2,Node(1,Empty(),Empty()),Empty()),Node(-1,Node(2,Empty(),Empty()),Empty())),Node(2,Node(1,Node(3,Empty(),Empty()),Empty()),Node(-1,Node(1,Empty(),Empty()),Empty())))
removeDuplicateBreadth(t1,t1)==(Empty(),Empty())
removeDuplicateBreadth(full4tree,generatedTree)==(full4tree,generatedTree)

// Zadanie 4 (5pkt)
sealed trait nlist[A]
case class Koniec[A]() extends nlist[A]
case class Element[A](elem: A,list: nlist[A]) extends nlist[A]

sealed  trait llist[A]
case class LKoniec[A]() extends llist[A]
case class LElement[A](elem: A,tail: ()=>llist[A]) extends llist[A]

def tonList[A](lxs: llist[A]): nlist[A]=
    lxs match {
      case LKoniec()=>Koniec()
      case LElement(elem, tail)=>Element(elem,tonList(tail()))
    }


def eachNElement[A](lxs: llist[A],n: Int, m: Int): llist[A]= {
    def inner[A](lxs: llist[A],p: Int, r: Int): llist[A]=
    (lxs,p,r) match {
      case (LKoniec(),_,_)=>LKoniec()
      case (LElement(_,_),_,0)=>LKoniec()
      case (LElement(x,xtl),1,_)=>LElement(x,()=>inner(xtl(),n,r-1))
      case (LElement(_,xtl),_,_)=>inner(xtl(),p-1,r-1)
    }
      (lxs,n,m) match {
        case (_,_,0)=>LKoniec()
        case (LKoniec(),_,_)=>LKoniec()
        case (LElement(x, xtl),_,_) => LElement(x, () => inner(xtl(),n, m-1))
      }
}

tonList(eachNElement(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),2,3))==Element(1,Element(3,Koniec()))
tonList(eachNElement(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),4,3))==Element(1,Koniec())
tonList(eachNElement(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),2,1))==Element(1,Koniec())
tonList(eachNElement(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),1,4))==Element(1,Element(2,Element(3,Koniec())))
eachNElement(LKoniec[Int](),1,1)==LKoniec()

// Zadanie 5 (5pkt)
def ldzialanie[A](lxs: llist[A],lys: llist[A],op: A=>A=>A): llist[A]={
    (lxs,lys) match {
      case(LKoniec(),_)=>lys
      case(_,LKoniec())=>lxs
      case (LElement(x,xtl),LElement(y,ytl))=>LElement(op(x)(y),()=>ldzialanie(xtl(),ytl(),op))
    }
}

tonList(ldzialanie(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),(a:Int)=>(b:Int)=>a+b))==
  Element(2,Element(4,Element(6,Koniec())))
tonList(ldzialanie(LElement(1,()=>LElement(2,()=>LKoniec[Int]())),LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),(a:Int)=>(b:Int)=>a+b))==
  Element(2,Element(4,Element(3,Koniec())))
tonList(ldzialanie(LElement(1,()=>LElement(2,()=>LElement(3,()=>LKoniec[Int]()))),LElement(1,()=>LElement(2,()=>LKoniec[Int]())),(a:Int)=>(b:Int)=>a-b))==
  Element(0,Element(0,Element(3,Koniec())))
