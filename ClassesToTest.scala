package List5

object ClassesToTest {

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Dog(namev: String, agev: Int) extends Debug {
    var name: String = namev
    var age: Int = agev
  }

  class PancakesRecipe(flourv: Double, glassOfMilkv: Int, eggsv: Int, authorv: String) extends BasicInformation {
    var flour: Double = flourv
    var glassOfMilk: Int = glassOfMilkv
    var eggs: Int = eggsv
    var author: String = authorv
  }

  class Country(namev: String, numberOfNeighboursv: Int, continentv: String) extends BasicInformation {
    var name: String = namev
    var numberOfNeighbours: Int = numberOfNeighboursv
    var continent: String = continentv
  }

  class Habitant(namev: String, nationalityv: Country) extends BasicInformation {
    var name: String = namev
    var nationality: Country = nationalityv
  }

  class Point2(xv: Int, yv: Int) extends BasicInformation {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

}
