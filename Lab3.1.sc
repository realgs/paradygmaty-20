def split (xs:List[Int]): (List[Int],List[Int])= {
  def negativeList(xs : List[Int], ys : List[Int]) :List[Int] =
    xs match {
      case Nil => ys
      case h :: t => if (h < 0) negativeList(t, h :: ys)
      else negativeList(t, ys)
    }

  def splitList(xs:List[Int],ys:List[Int],zs:List[Int]):(List[Int],List[Int])=
    xs match{
      case Nil=>(ys,zs)
      case h::t =>if(h%2==(-1)) splitList(t,h::ys,h::zs)
      else splitList(t,h::ys,zs)
    }
  splitList(negativeList(xs,List()),List(),List())
}

split(List(-3,-6,8,-9,13))
split(List(1,-2,3,-9,-5, -3))
split(List(-2,-4,6,-6,-1,-9))
















