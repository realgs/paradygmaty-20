package List5

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object CollectionElements {

  //Zadanie 1(2.5pkt)
  def duplicateElements[A](listToDuplicate: List[A], howManyDuplicates: LazyList[Int]): List[A] = {
    @tailrec
    def duplicateElementsInner(listToDuplicate: List[A], howManyDuplicates: LazyList[Int], counter: Int, resultList: List[A]): List[A] = {
        (listToDuplicate, howManyDuplicates) match {
          case (Nil, _) => resultList.reverse
          case (_, LazyList()) => resultList.reverse
          case (h :: t, hl #:: tl) => if (hl < 0) throw new Exception("You can't duplicate something negative times!")
                                      else if (counter < hl) duplicateElementsInner(listToDuplicate, howManyDuplicates, counter+1, h :: resultList)
                                      else duplicateElementsInner(t, tl, 0, resultList)
      }
    }
    duplicateElementsInner(listToDuplicate, howManyDuplicates, 0, Nil)
  }

  //Zadanie 2(2.5pkt)
  def collectionWithoutDuplicate[A](listToDuplicate: ListSet[A], howManyDuplicates: LazyList[Int]): List[A] = {
    @tailrec
    def collectionWithoutDuplicateInner(listToDuplicate: ListSet[A], howManyDuplicates: LazyList[Int], counter: Int, resultList: List[A]): List[A] = {
        if (listToDuplicate.isEmpty || howManyDuplicates.isEmpty) resultList.reverse
        else if (howManyDuplicates.head < 0) throw new Exception("You can't duplicate something negative times!")
        else if (counter < howManyDuplicates.head) collectionWithoutDuplicateInner(listToDuplicate, howManyDuplicates, counter+1, listToDuplicate.head :: resultList)
        else collectionWithoutDuplicateInner(listToDuplicate.tail, howManyDuplicates.tail, 0, resultList)
    }
    collectionWithoutDuplicateInner(listToDuplicate, howManyDuplicates, 0, Nil)
  }
}
