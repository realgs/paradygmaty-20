import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions

import scala.util.control.Exception

class l2Tests {
  @Test def testFunction1 {
    val function1 = new list2
    Assertions.assertEquals(12, function1.multiply(List(2.0, 2.0, 3.0)))
    Assertions.assertEquals(0, function1.multiply(List()))
    Assertions.assertEquals(-10.368, function1.multiply(List(-1.2, 2.4, 3.6)))
    Assertions.assertEquals(0, function1.multiply(List(-2.34, 3.456, -1, 0, 2)))
  }

  @Test def testFunction2 {
    val function2 = new list2
    Assertions.assertEquals("Ala!ma!kota.", function2.combineStrings(List("Ala", "ma", "kota"), "!", "."))
    Assertions.assertEquals("L__i__p__a!", function2.combineStrings(List("L", "i", "p", "a"), "__", "!"))
    Assertions.assertEquals("", function2.combineStrings(List(), "", ""))
    Assertions.assertEquals("", function2.combineStrings(List(), ".", ""))
    Assertions.assertEquals(".", function2.combineStrings(List(), "", "."))
    Assertions.assertEquals(" ", function2.combineStrings(List(" "), "", ""))
  }

  @Test def testFunction3: Unit = {
    val function3 = new list2
    Assertions.assertTrue(function3.checkNumbers(List(1.1, 2.2, 3.3, 4.01),0, 4.02))
    Assertions.assertTrue(function3.checkNumbers(List(),0, 1))
    Assertions.assertFalse(function3.checkNumbers(List(-25.23412, -100.231, 123.123), -10, -10))
    Assertions.assertTrue(function3.checkNumbers(List(1), 1, 1))
    Assertions.assertFalse(function3.checkNumbers(List(-123.124, -345, 123, 124.25, 36.346), -123.124, 124.25))

  }

  @Test def testFunction4: Unit = {
    val function4 = new list2
    Assertions.assertEquals(32, function4.pow(2,5))
    Assertions.assertEquals(20724.542789091483, function4.pow(5.241,6))
    Assertions.assertEquals(-0.03125, function4.pow(-2,-5))
    Assertions.assertEquals(1, function4.pow(12345,0))
    Assertions.assertEquals(4124.64, function4.pow(4124.64,1))
    Assertions.assertEquals(0, function4.pow(0,100))
  }

  @Test
  def functionThirdExceptionTest: Unit = {
    Assertions.assertThrows(classOf[Exception], () => (new list2).checkNumbers(List(4,64.12,421), 3, -8))
  }

  @Test
  def functionFourthExceptionTest: Unit = {
    Assertions.assertThrows(classOf[Exception], () => (new list2).pow(0, -1))
  }
}