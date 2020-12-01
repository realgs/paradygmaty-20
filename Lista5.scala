import java.lang.reflect.Field

object lab5{
  // Paweł Walkowiak

  // Zad1 (2.5pkt)
  def repeat(xs: List[Int],ys: List[Int]): List[Int]={
    if(xs.isEmpty||ys.isEmpty) List()
    else (xs.head,ys.head) match
    {
      case (_,0)=>repeat(xs.tail,ys.tail)
      case (x,y)=>x::repeat(xs,(y-1)::ys.tail)
    }
  }

  // Zad2 (2.5pkt)
  def notContains(ys: List[Int], elem: Int): Boolean=
    ys match {
      case List() => true
      case hd::tl => (hd!=elem)&&notContains(tl,elem)
    }

  def removeDuplicats(xs: List[Int]): List[Int] ={
    def checkList(checked: List[Int],toCheck: List[Int]): List[Int] ={
      toCheck match {
        case List() =>checked.reverse
        case hd::tl => if(notContains(checked,hd)) checkList(hd::checked,tl)
        else checkList(checked,tl)
      }
    }
    checkList(List(),xs)
  }
  removeDuplicats(List(1,2,3,5,6,1,2,7))

  def repeatWithoutDuplicates(xs: List[Int],ys: List[Int]): List[Int]=
    repeat(removeDuplicats(xs),ys)
}

// Zad3(5pkt)
trait Debug {
  def debugName: Unit = println("Class: " + getClass.getName)

  // Zad4(5pkt)
  def debugVars: Unit = {
    for (fields <- getClass.getDeclaredFields) {
      fields.setAccessible(true)
      println("Var: " + fields.getName + " => " + fields.getType + ", " + fields.get(this))
    }
  }
  // Zad5(5pkt)
  def getDebugName: String = getClass.getName

  def getDebugVars: (Array[Field], Array[AnyRef]) = {
    val v: Array[AnyRef] = new Array[AnyRef](getClass.getDeclaredFields.length)
    var i = 0;
    for (fields <- getClass.getDeclaredFields) {
      fields.setAccessible(true)
      v(i) = fields.get(this)
      i += 1
    }
    (getClass.getDeclaredFields, v)
  }
}
class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}