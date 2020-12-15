import scala.collection.immutable.Vector
import scala.annotation.tailrec

//Zadanie 1 (2.5pkt)
def duplicateVector[A](elem: Vector[A], rep: Vector[Int]): Vector[A] = {
  @tailrec
  def repeat(element: A, repetitions: Int, result: Vector[A]): Vector[A] = {
    if (repetitions == 0) result
    else repeat(element, repetitions - 1, element +: result)
  }

  elem match {
    case h +: t => if (rep.nonEmpty) repeat(h, rep.head, duplicateVector(t, rep.tail)) else Vector.empty
    case _ => Vector.empty
  }
}
duplicateVector(Vector(), Vector()) == Vector()
duplicateVector(Vector(1), Vector()) == Vector()
duplicateVector(Vector(), Vector(1)) == Vector()
duplicateVector(Vector(1, 2, 3), Vector(0, 3, 1, 4)) == Vector(2, 2, 2, 3)
duplicateVector(Vector(1, 2, 3, 4), Vector(0, 3, 1)) == Vector(2, 2, 2, 3)

//Zadanie 2 (2.5 pkt)
def duplicateSet[A](elem: Set[A], rep: Vector[Int]): Vector[A] = {
  @tailrec
  def repeat(element: A, repetitions: Int, result: Vector[A]): Vector[A] = {
    if (repetitions == 0) result
    else repeat(element, repetitions - 1, element +: result)
  }

  elem.toVector match {
    case h +: t => if (rep.nonEmpty) repeat(h, rep.head, duplicateVector(t, rep.tail)) else Vector.empty
    case _ => Vector.empty
  }
}

duplicateSet(Set(), Vector()) == Vector()
duplicateSet(Set(1, 1), Vector()) == Vector()
duplicateSet(Set(), Vector(1)) == Vector()
duplicateSet(Set(1, 2, 3), Vector(0, 3, 1, 4)) == Vector(2, 2, 2, 3)
duplicateSet(Set(1, 2, 3, 4), Vector(0, 3, 1)) == Vector(2, 2, 2, 3)
duplicateSet(Set(1, 1, 2, 3, 4), Vector(0, 3, 1, 2)) == Vector(2, 2, 2, 3, 4, 4)

//Zadanie 3 (5pkt) i 4 (5pkt)
trait Debug {
  def debugName(): Unit = {
    println("Class: " + this.getClass.getSimpleName)
  }

  def debugVars(): Unit = {
    this.getClass.getDeclaredFields.foreach(f => {
      f.setAccessible(true)
      if (f.getName != "$outer") println("Var: " + f.getName + " => " + f.getType.getSimpleName + " " + f.get(this))
    })
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

val p = new Point(3, 4)
p.debugName()
p.debugVars()

class Student(name: String, age: Int, index: String) extends Debug {
  var n = name
  var a = age
  var i = index
}

val s = new Student("Pawel", 24, "213225")
s.debugName()
s.debugVars()

//Zadanie 5 (5pkt)
trait Debug {
  def debugName: Class[_] = this.getClass

  def debugVars: List[(String, Class[_], Object)] = {
    var result = List.empty[(String,Class[_],Object)]
    val fields = this.getClass.getDeclaredFields
    for (i <- 0 to fields.length - 2) {
      fields(i).setAccessible(true)
      result = (fields(i).getName, fields(i).getType, fields(i).get(this)) :: result
    }
    result.reverse
  }
}

class Fruit(name: String, calories: Float, color: String) extends Debug {
  var n = name
  var c = calories
  var cl = color
}

val f = new Fruit("apple", 75.5f, "yellow")
f.debugName
f.debugVars
