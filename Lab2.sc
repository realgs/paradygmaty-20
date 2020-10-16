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

def Multiplication[A]( list:List[A]):Any =
  {
    if (list==Nil)
    {
        Nil
    }
    else {
      val string = list.mkString("")
      val string_replace = string.replaceAll("[.]","")
      if(string_replace.forall(_.isDigit))
      {
          doMultiPlication(list)
      }
      else
        {
          Nil
        }
    }
  }

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








