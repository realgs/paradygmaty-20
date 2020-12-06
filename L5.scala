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

def duplicateWithoutRepeats[T](coll: Queue[T], repeatsNumbers: Stack[Int]): List[T] = {
  val coll_no_rep = coll.distinct
  if(coll_no_rep.size == 0)
    List()
  else if(repeatsNumbers.size == 0){
    coll_no_rep.toList
  }
  else{
    repeatsNumbers.top match{
      case 0 => {
        coll_no_rep.dequeue() // O(1)
        repeatsNumbers.pop()  // O(1)
        duplicate(coll_no_rep, repeatsNumbers)
      }

      case num: Int => {
        repeatsNumbers.pop()    // O(1)
        repeatsNumbers.push(num - 1)  // O(1)
        coll_no_rep.front :: duplicate(coll_no_rep, repeatsNumbers)
      }
    }
  }
}

duplicateWithoutRepeats(Queue(1,2,3), Stack(0,2,2)) == List(2,2,3,3)
duplicateWithoutRepeats(Queue(1,2,3,4,5,6), Stack()) == List(1,2,3,4,5,6)
duplicateWithoutRepeats(Queue(9,9,9), Stack(0,3,0,9)) == List()
duplicateWithoutRepeats(Queue(), Stack(1,2,3)) == List()
duplicateWithoutRepeats(Queue(1,2,3), Stack(0,0,0)) == List()
duplicateWithoutRepeats(Queue(1,4,3,2,1,4,2,4), Stack(1,1,1,3,3,3,3,3)) == List(1,4,3,2,2,2)


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
  def debugName: String = getClass().toString
  def debugVars = {
    val fieldsTypeList = getClass().getDeclaredFields().toList

    def makeTupleList(fieldTypeL: List[Field]): List[(String, String, String)] =
      fieldTypeL match{
        case List() => List()
        case h :: l => {
          h.setAccessible(true)
          (h.getName, h.getType.toString, h.get(this).toString) :: makeTupleList(l)
        }
    }
    makeTupleList(fieldsTypeList)
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object Main{
  def main(args: Array[String]): Unit = {
    var p: Point = new Point(3, 4);
    val name = p.debugName;
    val vars = p.debugVars;
    0
  }
}
