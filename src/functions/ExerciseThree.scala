package functions

object ExerciseThree {
  def combineLists[A](firstList: List[A], secondList: List[A]): List[A] = {
    (firstList, secondList) match {
      case (Nil, _) => secondList
      case (_, Nil) => firstList
      case (h1 :: t1, _) => h1 :: combineLists(secondList, t1)
    }
  }

}
