import java.lang.reflect.{AnnotatedType, Field}

object l5 {
  //Zadanie 1 (2.5 pkt)
  def duplicate[A](xs: List[A], repetitions: List[Int]): List[A] =
    (xs, repetitions) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_ :: t1, 0 :: t2) => duplicate(t1, t2)
      case (h1 :: _, h2 :: t2) => h1 :: duplicate(xs, (h2 - 1) :: t2)
    }

  //Zadanie 2 (2.5 pkt)
  def duplicateUnique[A](xs: Set[A], repetitions: List[Int]): List[A] =
    (xs.toList, repetitions) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (_ :: t1, 0 :: t2) => duplicate(t1, t2)
      case (h1 :: _, h2 :: t2) => h1 :: duplicateUnique(xs, (h2 - 1) :: t2)
    }


  trait Debug {
    //Zadanie 3 (5 pkt)
    def debugName(): Unit =
      println("Class: " + getClass.getSimpleName)

    //Zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Var: " + field.getName + " => " + field.getAnnotatedType + ", " + field.get(this))
      }
    }

    //Zadanie 5 (5 pkt)
    def debugGetName(): String =
      getClass.getSimpleName

    def debugGetVars(): List[(String, String, Object)] = {
        val fields = getClass.getDeclaredFields
        var result = List.empty[(String, String, Object)]
        for (i <- 0 to fields.length - 1) {
          fields(i).setAccessible(true)
          result = (fields(i).getName, fields(i).getAnnotatedType.toString, fields(i).get(this)) :: result
        }
        result.reverse
    }

  }

}
