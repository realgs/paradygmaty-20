package Tests

import org.junit.jupiter.api.{BeforeEach, Nested, Test}
import org.junit.jupiter.api.Assertions._
import Source.Classes.{Dragon, Mug, MugOwner, Point}

import scala.collection.mutable


class Ex3_4_5Test {

  val dict: mutable.Map[String, (Class[_], Any)] = mutable.Map[String, (Class[_], Any)]()
  val intType: Class[_] = classOf[Int]
  val strType: Class[_] = classOf[String]


  @Test def testPointGetClass(): Unit =
  {
    val point = new Point(1, 2);
    assertEquals(("Class", "Point"), point.getClassName)
    point.debugName()
  }

  @Test def testDragonGetClass(): Unit =
  {
    val dragon = new Dragon("Dragon", "red", 2);
    assertEquals(("Class", "Dragon"), dragon.getClassName)
    dragon.debugName()
}

  @Test def testMugGetClass(): Unit =
  {
    val mug = new Mug();
    assertEquals(("Class", "Mug"), mug.getClassName)
    mug.debugName()
  }

  @Test def testMugOwnerGetClass(): Unit =
  {
    val mugOwner = new MugOwner(new Mug());
    assertEquals(("Class", "MugOwner"), mugOwner.getClassName)
    mugOwner.debugName()
  }

  @Nested
  class TestDebugVars
  {
    @BeforeEach def setupDict(): Unit =
    {
      dict.clear()
    }

    @Test def testPointGetVars(): Unit =
    {
      dict.put("x", (intType, 1))
      dict.put("y", (intType, 2))
      dict.put("a", (strType, "test"))
      val point = new Point(1, 2);
      assertEquals(dict, point.getVars)
      point.debugVars()
    }

    @Test def testMugGetVars(): Unit =
    {
      val mug = new Mug();
      assertEquals(dict, mug.getVars)
      mug.debugVars()
    }

    @Test def testDragonGetVars(): Unit =
    {
      dict.put("name", (strType, "Dragon2"))
      dict.put("color", (strType, "blue"))
      dict.put("age", (intType, 18))
      val dragon = new Dragon("Dragon2", "blue", 18)
      assertEquals(dict, dragon.getVars)
      dragon.debugVars()
    }

    @Test def testMugOwnerGetVars(): Unit =
    {
      val mug = new Mug()
      dict.put("mug", (mug.getClass, mug))
      val mugOwner = new MugOwner(mug)
      assertEquals(dict, mugOwner.getVars)
      println(mugOwner.getVars.get("mug"))
      mugOwner.debugVars()
    }
  }
}
