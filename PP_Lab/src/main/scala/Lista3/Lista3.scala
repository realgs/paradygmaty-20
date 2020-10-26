package Lista3
import scala.annotation.tailrec

object Lista3 extends App{
  // zadanie 1
  private def isNegative(number: Double): Boolean =
    number<0

  private def isNegativeAndOdd(number: Double): Boolean =
    ((number%2) != 0) && isNegative(number)

  def filterList(list: List[Double]): (List[Double], List[Double]) ={
    def innerFilterList(innerList: List[Double], filter: Double => Boolean): List[Double] =
      innerList match{
        case Nil => Nil
        case head::tail  => if(filter(head)) head::innerFilterList(tail, filter)  else innerFilterList(tail, filter)
      }
    (innerFilterList(list, isNegative), innerFilterList(list, isNegativeAndOdd))
  }


  // zadanie 2
  //zlozonosc obliczeniowa: O(n)
  //zlozonosc pamieciowa: 1 rekord aktywacji, ze względu na rekursje ogonową. Czyli O(1)
  def length[A](list: List[A]): Int = {
    @tailrec
    def innerLength[A](innerList: List[A], accum: Int): Int =
      if (innerList == Nil) accum
      else innerLength (innerList.tail, accum+1)
    innerLength(list, 0)
  }


  //zadanie 3
  //zlozonosc obliczeniowa: O(n), gdzie n to dlugosc ktorszej z list (tak na prawde to n+1, ponieważ jedna iteracja wykona sie raz na sam koniec, by dodac
  //                                                                                                                             na koniec dluzsza z liste)
  //zlozonosc pamieciowa: O(n), -,,-
  def merge[A](leftList: List[A], rightList: List[A]): List[A] =
    (leftList, rightList) match {
      case(headLeft::tailLeft, headRight::tailRight) => headLeft::headRight::merge(tailLeft, tailRight)
      case(Nil, rightList) => rightList
      case(leftList, Nil) => leftList
      case(Nil, Nil) => Nil
    }


  //zadanie 4
  def appendList[A](leftList: List[A], rightList: List[A]): List[A] = {
    (leftList, rightList) match {
      case (_, Nil) => leftList
      case (Nil, _) => rightList
      case (head::tail, _) => head::appendList(tail, rightList)
    }
  }

  def stringLength(string: String): Int = {
    @scala.annotation.tailrec
    def innerStringLength(innerString: String, accum: Int): Int =
      if (innerString == "") accum
      else innerStringLength(innerString.tail, accum+1)
    innerStringLength(string, 0)
  }

  def sliceStringFromBegin(string: String, endIndex: Int): (String, String) = {
    val stringSize = stringLength(string)
    if(endIndex >= stringSize) (string,"")
    else if (endIndex < 0) ("", string)
    else {
      @scala.annotation.tailrec
      def innerSliceString(word: String, accum: String, end: Int): (String, String) =
        if(end>=0) innerSliceString(word.tail, accum+word.head, end-1)
        else (accum, word)
      innerSliceString(string, "", endIndex)
    }
  }

  def ifContainsKey(word: String, key: String): Boolean = {
    val wordLength = stringLength(word)
    val keyLength = stringLength(key)
    if(wordLength < keyLength) false
    else{
      @scala.annotation.tailrec
      def innerContains(leftString: String, rightString: String): Boolean = {
        if(leftString=="") false
        else if(leftString.equals(key)) true
        else if(leftString!="" && rightString!="") innerContains(leftString.tail+rightString.head, rightString.tail)
        else false
      }
      val tuple = sliceStringFromBegin(word, keyLength-1)
      innerContains(tuple._1, tuple._2)
    }
  }

  def doesWordMatchAnyKey(word: String, keyList: List[String]): Boolean = {
    @scala.annotation.tailrec
    def innerDoesWordMatchAnyKey(word: String, keyList: List[String]): Boolean =
      keyList match{
        case(h::t) => if(ifContainsKey(word, h)) true  else innerDoesWordMatchAnyKey(word, t)
        case(Nil) => false
      }
    innerDoesWordMatchAnyKey(word, keyList)
  }

  //zwykla rekursja
  def find(inputList: List[String], keyWordsList: List[String]): List[String] = {
    def innerFind(inputList: List[String], keyWordsList: List[String]): List[String] =
      (inputList, keyWordsList) match{
        case(Nil, _) => List()
        case(_, Nil) => List()
        case(headI::tailI, headK::tailK) => if(doesWordMatchAnyKey(headI, keyWordsList)) headI::innerFind(tailI, keyWordsList)
                                            else innerFind(tailI, keyWordsList)
      }
    innerFind(inputList, keyWordsList)
  }

  //rekursja ogonowa
  def findTail(inputList: List[String], keyWordsList: List[String]): List[String] = {
    @scala.annotation.tailrec
    def innerFind(inputList: List[String], keyWordsList: List[String], accum: List[String]): List[String] =
      (inputList, keyWordsList) match{
        case(Nil, _) => accum
        case(_, Nil) => List()
        case(headI::tailI, headK::tailK) => if(doesWordMatchAnyKey(headI, keyWordsList)) innerFind(tailI, keyWordsList, appendList(accum, List(headI)))
                                            else innerFind(tailI, keyWordsList, accum)
      }
    innerFind(inputList, keyWordsList, List())
  }


  //zadanie 5
  //rekursja zwykla
  def joinLists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match{
      case(headFL::tailFL, _, _) => headFL::joinLists(tailFL, secondList, thirdList)
      case(Nil, headSL::tailSL, _)  => headSL::joinLists(Nil, tailSL, thirdList)
      case(Nil, Nil, headTL::tailTL) => headTL::joinLists(Nil, Nil, tailTL)
      case(Nil, Nil, Nil) => Nil
    }
  }

  //rekursja ogonowa
  def joinListsTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] ={
    @scala.annotation.tailrec
    def innerJoinListsTail[A](fL: List[A], sL: List[A], tL: List[A], accum: List[A]): List[A] =
      (fL, sL, tL) match{
        case(headFL::tailFL, _, _) => innerJoinListsTail(tailFL, sL, tL, appendList(accum, List(headFL)))
        case(Nil, headSL::tailSL, _)  => innerJoinListsTail(Nil, tailSL, tL, appendList(accum, List(headSL)))
        case(Nil, Nil, headTL::tailTL) => innerJoinListsTail(Nil, Nil, tailTL, appendList(accum, List(headTL)))
        case(Nil, Nil, Nil) => accum
      }
    innerJoinListsTail(firstList, secondList, thirdList, List())
  }
}
