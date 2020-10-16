// 1)


def Multiplication[A]( list:List[A]):Any =
  {
    def doMultiPlication[A](list:List[A]):Double =
    {
      if(list==Nil)
      {
        1
      }
      else
      {
        list.head.toString.toDouble * doMultiPlication(list.tail)
      }
    }

    def hasStringOnlyNumbers[B](list:List[B]):Boolean =
    {
      val string = list.mkString("")
      val string_replace = string.replaceAll("[.]","")
      if(string_replace.forall(_.isDigit))
      {
        return true
      }
      false
    }
    if (list==Nil)
    {
        Nil
    }
    else {

      if(hasStringOnlyNumbers(list))
      {
          doMultiPlication(list)
      }
      else
        {
          Nil
        }
    }
  }

//Tests
val list_double_first = List(2.5,4,10)

Multiplication(list_double_first)

val list_int_first = List(1,2,3.5)

Multiplication(list_int_first)

val list_1_element = List(1)

Multiplication(list_1_element)

val empty_list = List()

Multiplication(empty_list)

val list_not_number = List("aaa","bbb")

Multiplication(list_not_number)

val list_mixed = List(1,2,true,false,"aaa")

Multiplication(list_mixed)

// 2)
def SentenceBuilder(tuple:(List[String],Char,Char)):String=
  {
    def Sentence(tuple:(List[String],Char,Char)):String=
    {
      if(tuple._1==Nil)
      {
        "" + tuple._2
      }
      else if(tuple._1.length==1)
      {
        (tuple._1.head + Sentence(tuple._1.tail,tuple._2,tuple._3))
      }
      else
      {
        tuple._1.head + tuple._3 + Sentence(tuple._1.tail,tuple._2,tuple._3)
      }
    }
    if(tuple._1==Nil || tuple._2 == ' ')
      {
        ""
      }
    else
     {
        Sentence(tuple:(List[String],Char,Char))
     }
  }

val tuple_good = (List("ala","ma","kota"),'.',' ')
SentenceBuilder(tuple_good)

val tuple_string_empty = (List(),'.',' ')
SentenceBuilder(tuple_string_empty)

val tuple_wrong_end_sentence = (List(),' ',' ')
SentenceBuilder(tuple_wrong_end_sentence)

