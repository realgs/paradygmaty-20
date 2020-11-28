package Lista5

import java.lang.reflect.AnnotatedElement
import Lista5.{Car, Garage, Point, duplicate, duplicateWithoutRepeating}
import org.scalatest.FunSuite
import scala.collection.immutable.Queue

class Lista5Test extends FunSuite{

  val p: Point = new Point(3, 4)
  val car : Car = new Car("Mercedes", 2020)
  val garage : Garage = new Garage(new Car("Audi", 2015), 50)

  test("Lista 5 zadanie 1") {
    assert(duplicate(Queue(1,2,3), Queue(0,3,1,4)) == Queue(2,2,2,3))
    assert(duplicate(Queue(1,2,3), Queue(1,4)) == Queue(1,2,2,2,2))
    assert(duplicate(Queue('a','l','a'), Queue(1,2,-1)) == Queue('a','l','l'))
    assert(duplicate(Queue(), Queue(0,3,1,4)) == Queue())
    assert(duplicate(Queue(1,2,3), Queue()) == Queue())
  }

  test("Lista 5 zadanie 2") {
    assert(duplicateWithoutRepeating(Queue(1,2,3,1,2,4,3), Queue(0,3,1,4,5,2,4)) == Queue(2,2,2,3,4,4))
    assert(duplicateWithoutRepeating(Queue(1,2,1,2,1,2), Queue(4,-2)) == Queue(1,1,1,1))
    assert(duplicateWithoutRepeating(Queue(), Queue(0,3,1,4,5,2,4)) == Queue())
    assert(duplicateWithoutRepeating(Queue(1,2,3,1,2,4,3), Queue()) == Queue())
    assert(duplicateWithoutRepeating(Queue('a','l','a'), Queue(0,3,1)) == Queue('l','l','l'))
  }

  test("Lista 5 zadanie 3") {
    p.debugName()
    car.debugName()
    garage.debugName()
  }

  test("Lista 5 zadanie 4") {
    p.debugVars()
    println()
    car.debugVars()
    println()
    garage.debugVars()
  }

  test("Lista 5 zadanie 5") {
    assert(p.debugGetName() == "Point")
    assert(car.debugGetName() == "Car")
    assert(garage.debugGetName() == "Garage")

    debugGetVarsTest(p.debugGetVars())
    debugGetVarsTest(car.debugGetVars())
    debugGetVarsTest(garage.debugGetVars())
  }

  //used for testing debugGetVars() method from exc 5
  private def debugGetVarsTest(array: Array[(String, AnnotatedElement, Object)]) : Unit = {
    for(tuple <- array) {
      println("Var: " + tuple._1 + " => " + tuple._2 + ", " + tuple._3)
    }
    println()
  }

}
