import org.scalatest.flatspec.AnyFlatSpec

import scala.OOP.{Point, User}

class DebugTest extends AnyFlatSpec {
  "Debug trait collectClassNameAsString method" should "return name of the class as String" in {
    assert(new Point(3, 4).collectClassNameAsString() == "Point")
    assert(new User("Jan", "Nowak", 40, new Point(10, 20)).collectClassNameAsString() == "User")
  }

  "Debug trait collectDebugVarsAsMap method" should "return VectorMap where keys are fields' names and values are tuples with type and value)" in {
    assert(new Point(3, 4).collectDebugVarsAsMap().toString == "VectorMap(x -> (int,3), y -> (int,4), a -> (class java.lang.String,test))")
  }
}
