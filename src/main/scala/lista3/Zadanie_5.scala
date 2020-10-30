package lista3

import scala.annotation.tailrec

object Zadanie_5 {
  def joinListsRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    val listOfLists = List(list1, list2, list3)

    def helpFunction[A](list_of_lists: List[List[A]]): List[A] = {
      if (list_of_lists == Nil) Nil
      else
        list_of_lists.head ::: helpFunction(list_of_lists.tail)
    }

    helpFunction(listOfLists)
  }

  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    val listOfLists = List(list1, list2, list3)

    @tailrec
    def joinListsIter[A](list_of_lists: List[List[A]], accum: List[A]): List[A] = {
      if (list_of_lists == Nil) accum
      else joinListsIter(list_of_lists.tail, accum ::: list_of_lists.head)
    }

    joinListsIter(listOfLists, Nil)
  }
}
