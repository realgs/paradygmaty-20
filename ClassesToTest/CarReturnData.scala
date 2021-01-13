package L5.ClassesToTest
import L5.DebugReturnData
// class made for testing task 5
class CarReturnData (carBrand: String, modelOfCar: String, is4WheelDrive: Boolean, catalogPrice: Double) extends DebugReturnData {
  var brand: String = carBrand
  var model: String = modelOfCar
  var is4WD: Boolean = is4WheelDrive
  var price: Double = catalogPrice
}

