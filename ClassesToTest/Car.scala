package L5.ClassesToTest
import L5.Debug
// class made for testing tasks 3-4
class Car (carBrand: String, modelOfCar: String, is4WheelDrive: Boolean, catalogPrice: Double) extends Debug {
  var brand: String = carBrand
  var model: String = modelOfCar
  var is4WD: Boolean = is4WheelDrive
  var price: Double = catalogPrice
}

