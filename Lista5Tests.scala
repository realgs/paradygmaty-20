import org.scalatest.{FunSuite, _}

class RepeatTests extends FunSuite
{
  assert(lab5.repeat(List(1,2,3),List(0,3,1,4))==List(2,2,2,3))
  assert(lab5.repeat(List(),List(1,2,3))==List())
  assert(lab5.repeat(List(1,2,3),List())==List())
  assert(lab5.repeat(List(1,2,3,2),List(0,1,3,2))==List(2,3,3,3,2,2))
  assert(lab5.repeat(List(1,2,3,4),List(0,1,3))==List(2,3,3,3))
}
class RepeatWithoutDuplicatesTest extends FunSuite{
  assert(lab5.repeatWithoutDuplicates(List(1,2,3,1,2,3),List(0,3,1,4)) == List(2,2,2,3))
  assert(lab5.repeatWithoutDuplicates(List(1,2,3,1,2,3,4),List(0,3,1,4)) == List(2,2,2,3,4,4,4,4))
  assert(lab5.repeatWithoutDuplicates(List(),List(0,3,1,4)) == List())
  assert(lab5.repeatWithoutDuplicates(List(1,2,3,1,2,3),List()) == List())
  assert(lab5.repeatWithoutDuplicates(List(1,2,3,1,2,3),List(0,3,1,4)) == List(2,2,2,3))
}

class DebugTest extends FunSuite with Matchers {
  var p : Point = new Point(3,4);
  p.getDebugName should equal("Point")
  p.getDebugVars._2 should equal(Array(3,4,"test"))
  p.getDebugVars._1 should equal(p.getClass.getDeclaredFields)

  var p2 : Point = new Point(4,9);
  p2.getDebugName should equal("Point")
  p2.getDebugVars._2 should equal(Array(4,9,"test"))
  p2.getDebugVars._1 should equal(p2.getClass.getDeclaredFields)
}
