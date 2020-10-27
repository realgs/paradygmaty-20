import scala.annotation.tailrec

// zadanie 1
def split (xs:List[Int]): (List[Int],List[Int])= {
  @tailrec
  def findNegativeReverse(xs:List[Int],ys:List[Int]):List[Int]=
      xs match{
        case Nil=>ys
        case h::t=>if(h<0)findNegativeReverse(t,h::ys) else findNegativeReverse(t,ys)
      }
  @tailrec
  def splitIter(xs:List[Int],ys:List[Int],zs:List[Int]):(List[Int],List[Int])=
      xs match{
        case Nil=>(ys,zs)
        case h::t=>if(h%2==(-1)) splitIter(t,h::ys,h::zs)
        else splitIter(t,h::ys,zs)
      }
  splitIter(findNegativeReverse(xs,List()),List(),List())
}

split (List(-3,-6,8,-9,13))==(List(-3,-6,-9),List(-3,-9))
split (List(-3,8,-9,13))==(List(-3,-9),List(-3,-9))
split (List(8,13))==(List(),List())
split (Nil)==(Nil,Nil)

// zadanie 2
def length[A] (xs:List[A]): Int= {
  @tailrec
  def lengthIter[A](xs: List[A], counter: Int): Int =
    if (xs.nonEmpty) lengthIter(xs.tail, counter + 1) else counter
  lengthIter(xs, 0)
}

length(List(1,2,3))==3
length(Nil)==0
length(List('a','l','a'))==3

// zadanie 3
def connect[A](xs:List[A],ys:List[A]): List[A]= {
    @tailrec
    def pairReverse[A](xs:List[A],ys:List[A],zs:List[A]):(List[A],List[A])=
        (xs,ys) match{
          case (_,Nil)=>(zs,xs)
          case (Nil,_)=>(zs,ys)
          case (h1::t1,h2::t2)=>pairReverse(t1,t2,h2::h1::zs)
        }
    @tailrec
    def connectIter(zs:List[A],xs:List[A]):List[A]=
        zs match{
          case Nil=>xs
          case h::t=>connectIter(t,h::xs)
        }
    val (l,r)=pairReverse(xs,ys,List())
    connectIter(l,r)
}

connect(List(1,2,3,4),List(5,6,7,8))==List(1, 5, 2, 6, 3, 7, 4, 8)
connect(List(1,2,3),List(10,11,12,13,14,20,25))==List(1, 10, 2, 11, 3, 12, 13, 14, 20, 25)
connect(List(1,2,3,4),Nil)==List(1,2,3,4)
connect(Nil,List(1,2,3,4))==List(1,2,3,4)
connect(Nil,Nil)==Nil

// zadanie 4
def findPattern[A] (xs:List[A],patterns:List[A]): List[A]={
    @tailrec
    def findIter[A] (xs:List[A],patterns:List[A],ys:List[A]): List[A]=
        xs match{
          case Nil=>ys
          case h::t=> if(checkElem(h,patterns)) findIter(t,patterns,h::ys)
                      else findIter(t,patterns,ys)
        }
    @tailrec
    def checkElem[A](elem:A, patterns:List[A]): Boolean=
        patterns match{
          case Nil=> false
          case h::t=>myContains(elem.toString,h.toString) || checkElem(elem,t)
        }
    @tailrec
    def reverse(zs:List[A],ys:List[A]): List[A]=
        zs match {
          case Nil=>ys
          case h::t=>reverse(t,h::ys)
        }
    def myContains(elem:String,pat:String): Boolean={
        @tailrec
        def contIter(ls:List[Char],ps:List[Char]): Boolean={
          (ls,ps) match {
            case (_,Nil)=>true
            case (Nil,_)=>false
            case (hl::tl,hp::tp)=> if(hl==hp)contIter(tl,tp)
                                   else contIter(tl,pat.toList)
            }
        }
      contIter(elem.toList,pat.toList)
    }
  reverse(findIter(xs,patterns,List()),Nil)
}
findPattern(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168","index01692"))==List("index0168202", "index0168211", "index0168210", "index0169222","index0169224")
findPattern(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224" ),List("index0168"))==List("index0168202", "index0168211", "index0168210")
findPattern(Nil,Nil)==Nil
findPattern(Nil,List("a"))==Nil
findPattern(List("a","b","c","ba","ca","da","dd"),List("a","d"))==List("a","ba","ca","da","dd")
findPattern(List("a","b","c","ba","ca","da","dd"),List("a","d","c"))==List("a","c","ba","ca","da","dd")
findPattern(List(123,1345,223412,98765,32456,21345,123456),List(12,56))==List(123, 223412, 32456, 123456)

// zadanie 5
def joinLists[A](xs:List[A],ys:List[A],zs:List[A]):List[A]= {
  @tailrec
  def joinReverse[A](xs: List[A], ys: List[A], zs: List[A]): List[A] =
    (xs, ys) match {
      case (_,Nil) =>zs
      case (Nil,h :: t) => joinReverse(xs,t,h :: zs)
      case (h1 :: t1,_) => joinReverse(t1,ys, h1 :: zs)
    }
  @tailrec
  def joinFinaly[A](zs:List[A],gs:List[A]):List[A]=
    gs match {
      case Nil=>zs
      case h::t=>joinFinaly(h::zs,t)
    }
  if(ys==Nil) joinFinaly(ys,joinReverse(xs,zs,List()))
  else joinFinaly(zs,joinReverse(xs,ys,List()))
}

joinLists(List(5,4,3,2),List(1,0),List(9)) ==List(5, 4, 3, 2, 1, 0,9)
joinLists(Nil,Nil,Nil)==Nil
joinLists(List(1,2),Nil,List(3))==List(1,2,3)
joinLists(List(1,2),List(3),Nil)==List(1,2,3)

