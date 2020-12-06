//zad 1 2.5pkt
def addtolist(what: Int, howmany: Int, list: List[Int]):List[Int]={
  if(howmany==0)
    return list
  addtolist(what, howmany-1, list:::List(what))
}

def duplicate(list: List[Int], list2: List[Int]):List[Int]={
  def duplicateHelp(list: List[Int], list2: List[Int], list3: List[Int]): List[Int]={
    if(list.isEmpty || list2.isEmpty)
      return list3
    (list,list2) match {
      case (head::tail,head2::tail2) => return duplicateHelp(tail,tail2,list3:::addtolist(head,head2,List()))
    }
  }
  duplicateHelp(list,list2,List())
}

duplicate(List(1,2,3),List(0,3,1,4))

//zad 2 2.5pkt

def wasused(what: Int, list: List[Int]): Boolean = {
   list match{
    case(head::tail) =>{
      if(head==what)
        return true
      else wasused(what, tail)
    }
    case List() => return false
  }
}

def duplicate2(list: List[Int], list2: List[Int]):List[Int]={
  def duplicateHelp2(list: List[Int], list2: List[Int], list3: List[Int]): List[Int]={
    if(list.isEmpty || list2.isEmpty)
      return list3
    (list,list2) match {
      case (head::tail,head2::tail2) => {
        if(wasused(head,list3))
          return duplicateHelp2(tail,tail2,list3)
        else
          return duplicateHelp2(tail,tail2,list3:::addtolist(head,head2,List()))
      }
    }
  }
  duplicateHelp2(list,list2,List())
}

duplicate2(List(1,2,3,3),List(0,3,1,4))

//zad 3 5 pkt
trait Debug{
  def debugName() : Unit = {
    println("Class: " + getClass.getSimpleName)
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4);
p.debugName();

//zad 4 5 pkt
trait DebugVars{
  def debugVars(): Unit ={
    var fields = getClass.getDeclaredFields.dropRight(1)
    for(field <- fields){
      field.setAccessible(true)
      println("Var: "+ field.getName + " => " + field.get(this).getClass.getSimpleName + ", " + field.get(this))
    }
  }
}

class Point2(xv: Int, yv: Int) extends DebugVars {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p2 : Point2 = new Point2(3,4);
p2.debugVars();

//zad 5 5 pkt

trait returningDebug{
  def debug():(String,List[(String,String,String)])={
    val class_value = getClass.getSimpleName
    var list : List[(String,String,String)] = List();
    var fields = getClass.getDeclaredFields.dropRight(1)
    for(field <- fields){
      field.setAccessible(true)
      list = list:::List((field.getName,field.get(this).getClass.getSimpleName,field.get(this).toString))
    }
    return (class_value,list)
  }
}

class Point3(xv: Int, yv: Int) extends returningDebug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p3 : Point3 = new Point3(3,4);
p3.debug();