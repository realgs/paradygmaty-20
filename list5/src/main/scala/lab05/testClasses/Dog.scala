package lab05.testClasses

import lab05.Debug

class Dog(dogName: String, dogAge: Int) extends Debug {
  var name: String = dogName
  var age: Int = dogAge
  var vaccinated = false
}
