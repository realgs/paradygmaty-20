import org.scalatest.{FunSuite, Matchers}

class l5_Replicate_Test extends FunSuite {
  test("Replicate_Simple") {
    assert(l5.replicate(List(1, 2, 3))(List(1, 2, 3)) == List(1, 2, 2, 3, 3, 3))
  }

  test("Replicate_EmptyList") {
    assert(l5.replicate(List())(List(1, 2, 3)) == List())
  }

  test("Replicate_LongerHowManyTimes") {
    assert(l5.replicate(List(1, 2, 3))(List(1, 2, 3, 4, 5)) == List(1, 2, 2, 3, 3, 3))
  }
}

class l5_delete_Duplicates_Test extends FunSuite {
  test("delete_Duplicates_Simple") {
    assert(l5.delete_Duplicates(List(1, 2, 3))==List(1, 2, 3))
  }

  test("delete_Duplicates_EmptyList") {
    assert(l5.delete_Duplicates(List()) == List())
  }

  test("delete_Duplicates_1") {
    assert(l5.delete_Duplicates(List(1, 2, 3, 4, 5, 4, 1, 2)) == List(1, 2, 3, 4, 5))
  }

  test("delete_Duplicates_2") {
    assert(l5.delete_Duplicates(List(1, 2, 2, 3, 3, 1)) == List(1, 2, 3))
  }

  test("delete_Duplicates_Char") {
    assert(l5.delete_Duplicates(List('a', 'b', 'b', 'c')) == List('a', 'b', 'c'))
  }
}

class l5_replicate_No_Duplicates_Test extends FunSuite {
  test("Replicate_No_Duplicates_Simple") {
    assert(l5.replicate_No_Duplicates(List(1, 2, 3))(List(1, 2, 3)) == List(1, 2, 2, 3, 3, 3))
  }

  test("Replicate_No_Duplicates_EmptyList") {
    assert(l5.replicate_No_Duplicates(List())(List(1, 2, 3)) == List())
  }

  test("Replicate_No_Duplicates_LongerHowManyTimes") {
    assert(l5.replicate_No_Duplicates(List(1, 2, 3))(List(1, 2, 3, 4, 5)) == List(1, 2, 2, 3, 3, 3))
  }

  test("Replicate_No_Duplicates") {
    assert(l5.replicate_No_Duplicates(List(1, 2, 2, 3, 3, 1))(List(1, 2, 3)) == List(1, 2, 2, 3, 3, 3))
  }
  test("Replicate_No_Duplicates_LongerHowManyTimes2") {
    assert(l5.replicate_No_Duplicates(List(1, 2, 2, 3, 3, 1))(List(1, 2, 3, 4, 5)) == List(1, 2, 2, 3, 3, 3))
  }
}

object Task3_Test extends App {
  var p : Point = new Point(3,4);
  p.debug_Name();
}

object Task4_Test extends App {
  var p : Point2 = new Point2(3,4);
  p.debug_Vars();
}

object Task5_Test extends App {
  var p : Point3 = new Point3(3,4);
  println(p.get_Debug_Name)
  for (i <- 0 to p.get_Debug_Vars._1.length-1) {
    println("Var: " + p.get_Debug_Vars._1(i) + " => " + p.get_Debug_Vars._2(i) + ", " + p.get_Debug_Vars._3(i))
  }
}

class l5_get_Debug_Vars_Name_Test extends FunSuite with Matchers {
  test("get_Debug_Vars_Test") {
    var p : Point3 = new Point3(3,4);
    p.get_Debug_Name should equal("Point3")
    p.get_Debug_Vars._1 should equal(List("x", "y", "a"))
    var ret_list_Type = List[String]()
    for (i <- 0 to p.get_Debug_Vars._1.length-1) {
      ret_list_Type = p.get_Debug_Vars._2(i).toString()::ret_list_Type
    }
    ret_list_Type.reverse should equal(List("int", "int", "class java.lang.String"))
    p.get_Debug_Vars._3 should equal(List(3, 4, "test"))
  }
}


