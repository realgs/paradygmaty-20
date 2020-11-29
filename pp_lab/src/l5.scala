import java.lang.reflect.Field

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
  def duplicateUnique[A](xs: List[A], repetitions: List[Int]): List[A] =
    duplicate(xs.distinct, repetitions)


  trait Debug {
    //Zadanie 3 (5 pkt)
    def debugName(): Unit =
      println("Class: " + getClass.getName)

    //Zadanie 4 (5 pkt)
    def debugVars(): Unit = {
      def printFields(fields: Array[Field]): Unit = {
        fields.foreach(field => {
          field.setAccessible(true)
          println("Var " + field.getName + " => " + field.getType + ", " + field.get(this))
        })
      }
      val fields = this.getClass.getDeclaredFields
      printFields(fields.slice(0, fields.length - 1))
    }

    //Zadanie 5 (5 pkt)
  }

}
