class L2 {
  def productOfList(xs:List[Int]): Int={
    if(xs==Nil)
      0
    else{
      if(xs.tail!=Nil)
        xs.head*productOfList(xs.tail)
      else
        xs.head
    }
  }

  def makeSentence(xs:List[String],separator:Char,end:Char): String={
    if(xs==Nil)
      ""
    else{
      if(xs.tail!=Nil)
        xs.head+separator+makeSentence(xs.tail,separator,end)
      else
        xs.head+end
    }
  }

  def interval(xs:List[Double],x:Double,y:Double): Boolean={
    if(xs==Nil) new Exception("list is empty")
    if(xs.tail!=Nil)
      ((xs.head>=x&&xs.head<=y)||(xs.head>=y&&xs.head<=x))&&interval(xs.tail,x,y)
    else
      (xs.head>=x&&xs.head<=y)||(xs.head>=y&&xs.head<=x)
  }

  def power(base:Double,pw:Int): Double= {
    if (pw == 0)
      1
    else {
      if (pw < 0) 
        (1 / base) * power(1/base, -pw - 1)
      else 
        base * power(base, pw - 1)
    }
  }
}
