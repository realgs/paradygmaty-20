package List5

import scala.annotation.tailrec

trait BasicInformation {

  //Zadanie 5(5pkt)
  def getClassName: String =
    getClass.getSimpleName

  def getInformation: List[(String, Class[_], Object)] = {
    val fields = getClass.getDeclaredFields
    val resultList: List[(String, Class[_], Object)] = List()
    var i = -1
    @tailrec
    def getInformationInner(condition: => Boolean, resultList: List[(String, Class[_], Object)])(body: => (String, Class[_], Object)): List[(String, Class[_], Object)] =
      if(condition) {
        getInformationInner(condition, body :: resultList)(body)
      } else resultList.reverse

    getInformationInner(i < fields.size-1, resultList) {
      i += 1
      fields(i).setAccessible(true)
      (fields(i).getName, fields(i).getType, fields(i).get(this))
    }
  }

  //Metoda na potrzebę testów
  def printList(list: List[(String, Class[_], Object)]): Unit = {
    if (list.nonEmpty) {
      println("Var: " + list.head._1 + " => " + list.head._2 + ", " + list.head._3)
      printList(list.tail)
    }
  }

}
