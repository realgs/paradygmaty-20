package List5Test
import org.scalatest.FunSuite
import L5.ModifyCollection
import scala.collection.immutable.{ListSet, Queue}
import L5.ClassesToTest.{Car, City, Point, CarReturnData, CityReturnData, PointReturnData}

class L5Test extends FunSuite {
  test("test of helper function 'addNTimes'") {
    assert(ModifyCollection.addNTimes(Queue(1, 2, 3), 4, 3) == Queue(1, 2, 3, 4, 4, 4))
    assert(ModifyCollection.addNTimes(Queue("e1", "e2", "e3", "e4", "e5"), "e6", 1) == Queue("e1", "e2", "e3", "e4", "e5", "e6"))
    assert(ModifyCollection.addNTimes(Queue(), 2.5, 2) == Queue(2.5, 2.5))
    assert(ModifyCollection.addNTimes(Queue(true, false), false, 2) == Queue(true, false, false, false))
    assertThrows[Exception](ModifyCollection.addNTimes(Queue(1.5, 2.6, 3.7), 4.8, -6))
  }

  test("test of task 1 -> function 'duplicateElements'") {
    assert(ModifyCollection.duplicateElements(List(1, 1, 1), List(1, 2, 5)) == Queue(1, 1, 1, 1, 1, 1, 1, 1))
    assert(ModifyCollection.duplicateElements(List(1, 2, 1), List(1, 2, 5)) == Queue(1, 2, 2, 1, 1, 1, 1, 1))
    assert(ModifyCollection.duplicateElements(List(1, 2, 3), List(2, 3, 4)) == Queue(1, 1, 2, 2, 2, 3, 3, 3, 3))
    assert(ModifyCollection.duplicateElements(List(1, 2, 3), List(2, 3, 4, 6, 7)) == Queue(1, 1, 2, 2, 2, 3, 3, 3, 3))
    assert(ModifyCollection.duplicateElements(List("e1", "e2", "e3", "e4", "e5"), List(1, 0, 3)) == Queue("e1", "e3", "e3", "e3"))
    assert(ModifyCollection.duplicateElements(Nil, Nil) == Queue())
    assert(ModifyCollection.duplicateElements(Nil, List(1, 2, 3)) == Queue())
    assertThrows[Exception](ModifyCollection.duplicateElements(List(1.5, 2.6, 3.7), List(2, -1, 6)))
  }

  test("test of task 2 -> function 'duplicateElements'") {
    assert(ModifyCollection.duplicateElements(ListSet(1, 1, 1), List(1, 2, 5)) == Queue(1))
    assert(ModifyCollection.duplicateElements(ListSet(1, 2, 1), List(1, 2, 5)) == Queue(1, 2, 2))
    assert(ModifyCollection.duplicateElements(ListSet(1, 2, 3), List(2, 3, 4)) == Queue(1, 1, 2, 2, 2, 3, 3, 3, 3))
    assert(ModifyCollection.duplicateElements(ListSet(1, 2, 3), List(2, 3, 4, 6, 7)) == Queue(1, 1, 2, 2, 2, 3, 3, 3, 3))
    assert(ModifyCollection.duplicateElements(ListSet("e1", "e2", "e3", "e4", "e5"), List(1, 0, 3)) == Queue("e1", "e3", "e3", "e3"))
    assert(ModifyCollection.duplicateElements(ListSet(), Nil) == Queue())
    assert(ModifyCollection.duplicateElements(ListSet(), List(1, 2, 3)) == Queue())
    assertThrows[Exception](ModifyCollection.duplicateElements(ListSet(1.5, 2.6, 3.7), List(2, -1, 6)))
  }

  test("test of task 3 -> function 'debugName'") {
    val point1 = new Point(1,2)
    val point2 = new Point(10, -20)
    point1.debugName()
    point2.debugName()
    val car1 = new Car("Lexus", "NX", true, 175900)
    val car2 = new Car("Porsche", "Cayenne", true, 380000)
    car1.debugName()
    car2.debugName()
    val city1 = new City("London", 1572)
    val city2 = new City("Warszawa", 517.2)
    city1.debugName()
    city2.debugName()
  }

  test("test of task 4 -> function 'debugVars'") {
    val point1 = new Point(1,2)
    val point2 = new Point(10, -20)
    point1.debugVars()
    point2.debugVars()
    val car1 = new Car("Lexus", "NX", true, 175900)
    val car2 = new Car("Porsche", "Cayenne", true, 380000)
    car1.debugVars()
    car2.debugVars()
    val city1 = new City("London", 1572)
    val city2 = new City("Warszawa", 517.2)
    city1.debugVars()
    city2.debugVars()
  }

  test("test of task 5") {
    val point1 = new PointReturnData(1,2)
    assert(point1.getName() == "PointReturnData")
    assert(point1.getName_Type_ValueOfFieldsList() == List(("x", classOf[Int], 1), ("y", classOf[Int], 2), ("a", classOf[String], "test")))

    val point2 = new PointReturnData(10, -20)
    assert(point2.getName() == "PointReturnData")
    assert(point2.getName_Type_ValueOfFieldsList() == List(("x", classOf[Int], 10), ("y", classOf[Int], -20), ("a", classOf[String], "test")))

    val car1 = new CarReturnData("Lexus", "NX", true, 175900)
    assert(car1.getName() == "CarReturnData")
    assert(car1.getName_Type_ValueOfFieldsList() == List(("brand", classOf[String], "Lexus"), ("model", classOf[String], "NX"), ("is4WD", classOf[Boolean], true), ("price", classOf[Double], 175900)))

    val car2 = new CarReturnData("Porsche", "Cayenne", true, 380000)
    assert(car2.getName() == "CarReturnData")
    assert(car2.getName_Type_ValueOfFieldsList() == List(("brand", classOf[String], "Porsche"), ("model", classOf[String], "Cayenne"), ("is4WD", classOf[Boolean], true), ("price", classOf[Double], 380000)))

    val city1 = new CityReturnData("London", 1572)
    assert(city1.getName() == "CityReturnData")
    assert(city1.getName_Type_ValueOfFieldsList() == List(("name", classOf[String], "London"), ("area", classOf[Double], 1572)))

    val city2 = new CityReturnData("Warszawa", 517.2)
    assert(city2.getName() == "CityReturnData")
    assert(city2.getName_Type_ValueOfFieldsList() == List(("name", classOf[String], "Warszawa"), ("area", classOf[Double], 517.2)))
  }
}

