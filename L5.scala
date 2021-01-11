//zadanie 1 (2.5 pkt)

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

def duplicate[T](coll: Queue[T], repeatsNumbers: Stack[Int]): List[T] =
  if(coll.size == 0)
    List()
  else if(repeatsNumbers.size == 0){
    coll.toList
  }
  else{
    repeatsNumbers.top match{
      case 0 => {
        coll.dequeue() // O(1)
        repeatsNumbers.pop()  // O(1)
        duplicate(coll, repeatsNumbers)
      }
      case num: Int => {
        repeatsNumbers.pop()    // O(1)
        repeatsNumbers.push(num - 1)  // O(1)
        coll.front :: duplicate(coll, repeatsNumbers)
      }
    }
  }

duplicate(Queue(1,2,3), Stack(0,2,2)) == List(2,2,3,3)
duplicate(Queue(1,2,3,4,5,6), Stack()) == List(1,2,3,4,5,6)
duplicate(Queue(9,9,9), Stack(0,3,0,9)) == List(9,9,9)
duplicate(Queue(), Stack(1,2,3)) == List()
duplicate(Queue(1,2,3), Stack(0,0,0)) == List()


//zadanie 2 (2.5 pkt)
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.Stack

def duplicateWithoutRepeats[T](coll: LinkedHashSet [T], repeatsNumbers: Stack[Int]): List[T] = {
  if(coll.size == 0)
    List()
  else if(repeatsNumbers.size == 0){
    coll.toList
  }
  else{
    repeatsNumbers.top match{
      case 0 => {
        coll -= coll.head
        repeatsNumbers.pop()
        duplicateWithoutRepeats(coll, repeatsNumbers)
      }
      case num: Int => {
        repeatsNumbers.pop()
        repeatsNumbers.push(num - 1)
        coll.head :: duplicateWithoutRepeats(coll, repeatsNumbers)
      }
    }
  }
}

duplicateWithoutRepeats(LinkedHashSet(1,2,3), Stack(0,2,2)) == List(2,2,3,3)
duplicateWithoutRepeats(LinkedHashSet (1,2,3,4,5,6), Stack()) == List(1,2,3,4,5,6)
duplicateWithoutRepeats(LinkedHashSet (9,9,9), Stack(0,3,0,9)) == List()
duplicateWithoutRepeats(LinkedHashSet(), Stack(1,2,3)) == List()
duplicateWithoutRepeats(LinkedHashSet(1,2,3), Stack(0,0,0)) == List()
duplicateWithoutRepeats(LinkedHashSet(1,4,3,2,1,4,2,4), Stack(1,1,1,3,3,3,3,3)) == List(1,4,3,2,2,2)
duplicateWithoutRepeats(LinkedHashSet(3,2,1), Stack(2,1,1)) == List(3,3,2,1)


//zadanie 3 (5 pkt)
//zadanie 4 (5 pkt)

trait Debug{
  def debugName = println(getClass())
  
  def debugVars = {
    val fieldsTypeArray = getClass().getDeclaredFields()
    for(i <- 0 to fieldsTypeArray.length - 1){
      fieldsTypeArray(i).setAccessible(true)
      println("Var: " + fieldsTypeArray(i).getName + " => " + fieldsTypeArray(i).getType + ", " + fieldsTypeArray(i).get(this))
    }
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object Main{
  def main(args: Array[String]): Unit ={
    var p : Point = new Point(3,4);
    p.debugName;
    p.debugVars;
  }
}


//zadanie 5 (5 pkt)
import java.lang.reflect.Field

trait Debug{
  def debugName: Class[_ <: Debug] = getClass()

  def debugVars: List[(String, Class[_] , Object)] = {
    val fieldsTypeList = getClass().getDeclaredFields().toList
    def makeTupleList(fieldTypeL: List[Field]): List[(String, Class[_] , Object)] =
      fieldTypeL match{
        case List() => List()
        case h :: l => {
          h.setAccessible(true)
          (h.getName, h.getType, h.get(this)) :: makeTupleList(l)
        }
      }
    makeTupleList(fieldsTypeList)
  }
}

//test classes:
class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

class Test() extends Debug{
  var p: Point = new Point(1,2)
  val t: Double = 4.5
}

class Test2() extends Debug{
}

object Main{
  def main(args: Array[String]): Unit = {
    var p: Point = new Point(3, 4);
    println(p.debugName == classOf[Point])
    println(p.debugVars == List(("x", classOf[Int], 3), ("y", classOf[Int], 4), ("a", classOf[String], "test")))

    p = new Point(0, 0)
    println(p.debugName == classOf[Point])
    println(p.debugVars == List(("x", classOf[Int], 0), ("y", classOf[Int], 0), ("a", classOf[String], "test")))

    val t: Test = new Test()
    val t_p = t.p
    println(t.debugName == classOf[Test])
    println(t.debugVars == List(("p", classOf[Point], t_p), ("t", classOf[Double], 4.5)))

    val s: Test2 = new Test2()
    println(s.debugName == classOf[Test2])
    println(s.debugVars == List())
  }
}
