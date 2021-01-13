package Tests


import org.junit.jupiter.api.{BeforeEach, Nested, Test}
import org.junit.jupiter.api.Assertions._
import Source.Classes.{Dragon, Mug, MugOwner, Point}
import scala.collection.mutable


class Ex3_4_5Test {

  @Test def testPointGetClass(): Unit =
  {
    val point = new Point(1, 2)
    assertEquals("Point", point.getClassName)
  }

  @Test def testDragonGetClass(): Unit =
  {
    val dragon = new Dragon("Dragon", "red", 2)
    assertEquals("Dragon", dragon.getClassName)
}

  @Test def testMugGetClass(): Unit =
  {
    val mug = new Mug()
    assertEquals("Mug", mug.getClassName)
  }

  @Test def testMugOwnerGetClass(): Unit =
  {
    val mugOwner = new MugOwner(new Mug())
    assertEquals("MugOwner", mugOwner.getClassName)
  }

  @Nested
  class TestDebugVars
  {

    val dict: mutable.Map[String, (Class[_], Any)] = mutable.Map[String, (Class[_], Any)]()
    val intType: Class[_] = classOf[Int]
    val strType: Class[_] = classOf[String]

    @BeforeEach def setupDict(): Unit =
    {
      dict.clear()
    }

    @Test def testPointGetVars(): Unit =
    {
      dict.put("x", (intType, 1))
      dict.put("y", (intType, 2))
      dict.put("a", (strType, "test"))
      val point = new Point(1, 2)
      assertEquals(dict, point.getVars)
    }

    @Test def testMugGetVars(): Unit =
    {
      val mug = new Mug()
      assertEquals(dict, mug.getVars)
    }

    @Test def testDragonGetVars(): Unit =
    {
      dict.put("name", (strType, "Dragon2"))
      dict.put("color", (strType, "blue"))
      dict.put("age", (intType, 18))
      val dragon = new Dragon("Dragon2", "blue", 18)
      assertEquals(dict, dragon.getVars)
    }

    @Test def testMugOwnerGetVars(): Unit =
    {
      val mug = new Mug()
      dict.put("mug", (mug.getClass, mug))
      val mugOwner = new MugOwner(mug)
      assertEquals(dict, mugOwner.getVars)
    }
  }
}
