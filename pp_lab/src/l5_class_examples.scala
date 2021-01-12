import l5.Debug

object l5_class_examples {
  class Point(givenX: Double,  givenY: Double) extends Debug {
    var x: Double = givenX
    var y: Double = givenY
  }

  class Student(givenName: String, givenAge: Int, givenIndex: String, givenGender: Char) extends Debug {
    var name: String = givenName
    var age: Int = givenAge
    var index: String = givenIndex
    var gender: Char = givenGender
  }
}
