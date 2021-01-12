package scala.OOP

// used for Debug trait tests
class User(firstNameParam: String, lastNameParam: String, ageParam: Int, favouritePointParam: Point) extends Debug {
  var firstName: String = "Jan"
  var lastName: String = "Kowalski"
  var age: Int = 20
  var favouritePoint: Point = new Point(2, 3)
}
