import org.scalatest.FunSuite
import List5.CollectionElements
import List5.ClassesToTest.{Habitant, Country, PancakesRecipe, Point2, Point, Dog}
import scala.collection.immutable.ListSet

class L5Test extends FunSuite {

  test("Test of 'duplicateElements' function: ") {
    assert(CollectionElements.duplicateElements(List(1,2,3), LazyList(0,3,1,4)) == List(2,2,2,3))
    assert(CollectionElements.duplicateElements(List(), LazyList(9,8,7,6)) == List())
    assert(CollectionElements.duplicateElements(List('a','b','c','d'), LazyList(1,2,3)) == List('a','b','b','c','c','c'))
    assert(CollectionElements.duplicateElements(List(2.7, 0.9), LazyList()) == List())
    assert(CollectionElements.duplicateElements(List("Ala", "ma", "kota"), LazyList(0,0,4)) == List("kota", "kota", "kota", "kota"))
    assertThrows[Exception](CollectionElements.duplicateElements(List(5,6,7), LazyList(1, -3, 0)))
  }

  test("Test for 'elementsWithoutDuplicates' function: ") {
    assert(CollectionElements.collectionWithoutDuplicate(ListSet(1,1,1,4,5,7), LazyList(3, 0, 1)) == List(1,1,1,5))
    assert(CollectionElements.collectionWithoutDuplicate(ListSet(1.9,0,8.7,1.9,-3.43), LazyList(3,1,1,2,5)) == List(1.9,1.9,1.9,0,8.7,-3.43,-3.43))
    assert(CollectionElements.collectionWithoutDuplicate(ListSet(), LazyList(8,9)) == List())
    assertThrows[Exception](CollectionElements.collectionWithoutDuplicate(ListSet('a','b','c','d'), LazyList(0,-9,4,5)))
    assert(CollectionElements.collectionWithoutDuplicate(ListSet('a','b','c','d'), LazyList(0,1)) == List('b'))
  }

  test("Test for exercise 3 and 4:") {
    val point: Point = new Point(3,4)
    point.debugName()
    point.debugVars()

    val dog: Dog = new Dog("Szarik", 12)
    dog.debugName()
    dog.debugVars()
  }

  test("Test for exercise 5:") {
    val country: Country = new Country("Poland", 6, "Europe")
    assert(country.getClassName == "Country")
    assert(country.getInformation == List(("name", classOf[String], "Poland"), ("numberOfNeighbours", classOf[Int], 6), ("continent", classOf[String], "Europe")))
    country.printList(country.getInformation)

    val habitantOfPoland: Habitant = new Habitant("Anonymous", country)
    assert(habitantOfPoland.getClassName == "Habitant")
    assert(habitantOfPoland.getInformation == List(("name", classOf[String], "Anonymous"), ("nationality", classOf[Country], country)))
    habitantOfPoland.printList(habitantOfPoland.getInformation)

    val point2: Point2 = new Point2(3,4)
    assert(point2.getClassName == "Point2")
    assert(point2.getInformation == List(("x", classOf[Int], 3), ("y", classOf[Int], 4), ("a", classOf[String], "test")))
    point2.printList(point2.getInformation)

    val pancakesRecipe: PancakesRecipe = new PancakesRecipe(45.9, 2, 2, "Anonymous")
    assert(pancakesRecipe.getClassName == "PancakesRecipe")
    assert(pancakesRecipe.getInformation == List(("flour", classOf[Double], 45.9), ("glassOfMilk", classOf[Int], 2), ("eggs", classOf[Int], 2), ("author", classOf[String], "Anonymous")))
    pancakesRecipe.printList(pancakesRecipe.getInformation)
  }

}
