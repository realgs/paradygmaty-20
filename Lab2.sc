import scala.math

//Auxiliary function
def hasListOnlyNumbers[A](list:List[A]):Boolean = {

  if(list==Nil)
  {
    return false
  }
  val string = list.mkString("")
  val string_replace = string.replaceAll("[.]|[-]", "")
  if (string_replace.forall(_.isDigit)) {
    return true
  }
  false
}

// 1)

def multiplication[A]( list:List[A]):Any =
  {
    def doMultiPlication[B](list:List[B]):Double =
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

    if (list==Nil)
    {
        Nil
    }
    else {

      if(hasListOnlyNumbers(list))
      {
          doMultiPlication(list)
      }
      else
        {
          Nil
        }
    }
  }

//Tests 1
val list_double_first = List(2.5,4,10)
multiplication(list_double_first)

val list_int_first = List(1,2,3.5)
multiplication(list_int_first)

val list_1_element = List(1)
multiplication(list_1_element)

val empty_list = List()
multiplication(empty_list)

val list_not_number = List("aaa","bbb")
multiplication(list_not_number)

val list_mixed = List(1,2,true,false,"aaa")
multiplication(list_mixed)

// 2)
def sentenceBuilder(tuple:(List[String],Char,Char)):String=
  {
    def sentence(tuple:(List[String],Char,Char)):String=
    {
      if(tuple._1==Nil)
      {
        "" + tuple._2
      }
      else if(tuple._1.length==1)
      {
        (tuple._1.head + sentence(tuple._1.tail,tuple._2,tuple._3))
      }
      else
      {
        tuple._1.head + tuple._3 + sentence(tuple._1.tail,tuple._2,tuple._3)
      }
    }
    if(tuple._1==Nil || tuple._2 == ' ')
      {
        ""
      }
    else
     {
        sentence(tuple:(List[String],Char,Char))
     }
  }

//Tests 2

val task_2_tuple_good = (List("ala","ma","kota"),'.',' ')
sentenceBuilder(task_2_tuple_good)

val task_2_tuple_single_element_in_list = (List("ala"),'.',' ')
sentenceBuilder(task_2_tuple_single_element_in_list)

val task_2_tuple_string_empty = (List(),'.',' ')
sentenceBuilder(task_2_tuple_string_empty)

val task_2_tuple_wrong_end_sign= (List("ala","ma","kota"),' ',' ')
sentenceBuilder(task_2_tuple_wrong_end_sign)

//3

def isInCompartment[A]( tuple:(List[A],A,A)):Boolean =
{
  def makeCompartment(tuple:(Double,Double)):(Double,Double) =
    {
      if(tuple._1<tuple._2) {

          return (tuple._1,tuple._2)
      }
      (tuple._2,tuple._1)
    }

  def makeListOfDoubles[B](list:List[B]):List[Double] ={
    if(list==Nil) {
      return List()
    }
    list.head.toString.toDouble :: makeListOfDoubles(list.tail)
  }

  val ListFromCompartment = List(tuple._2,tuple._3)

  if(hasListOnlyNumbers(tuple._1) && hasListOnlyNumbers(ListFromCompartment))
  {
    val compartment = makeCompartment((tuple._2.toString.toDouble,tuple._3.toString.toDouble))
    val list_of_doubles = makeListOfDoubles(tuple._1)
    if(list_of_doubles.forall(p => {p<=compartment._2 && p>=compartment._1})) {
        return true
    }
    false
  }
  else false
}

//Tests 3

val task_3_tuple_good = (List(5,8,7,-1),8,-1)
isInCompartment(task_3_tuple_good)

val task_3_tuple_with_some_doubles = (List(7,3.1),8,3)
isInCompartment(task_3_tuple_with_some_doubles)

val task_3_tuple_with_more_doubles = (List(3.2,3.1),3,3.3)
isInCompartment(task_3_tuple_with_some_doubles)

val task_3_tuple_good_list_wrong = (List(6,3.2,7),4,8)
isInCompartment(task_3_tuple_good_list_wrong)

val task_3_tuple_empty_list= (List(),2,6)
isInCompartment(task_3_tuple_empty_list)

val task_3_tuple_wrong_types = (List(true),2,6)
isInCompartment(task_3_tuple_wrong_types)

val task_3_tuple_wrong_compartment = (List(6,6),"a",6)
isInCompartment(task_3_tuple_wrong_types)

//4
def exponentiation[A](tuple:(A,A)):Double=
{
  def rec(tuple:(Double,Double)):Double=
    {
      if(tuple._2==0)
      {
        1
      }
      else
      {
        tuple._1*exponentiation((tuple._1,tuple._2-1))
      }
    }

  val list = List(tuple._1,tuple._2)
  if(hasListOnlyNumbers(list) && (tuple._2.toString.toDouble % 1) == 0) {
    if(tuple._2.toString.toDouble<0)
      {
        val value = 1/tuple._1.toString.toDouble
        val value_2 = -1*tuple._2.toString.toDouble
         rec((value,value_2))
      }
    else
      {
        rec((tuple._1.toString.toDouble,tuple._2.toString.toDouble))
      }
  }
  else
    0
}

//tests 4
val tuple_4_1 = (3,5)
exponentiation(tuple_4_1)
scala.math.pow(3,5)

val tuple_4_2 = ("a",4)
exponentiation(tuple_4_2)

val tuple_4_3 = (2,true)
exponentiation(tuple_4_3)

val tuple_4_4 = (Nil,6)
exponentiation(tuple_4_4)

val tuple_4_5 = (2,-3)
exponentiation(tuple_4_5)
scala.math.pow(2,-3)

val tuple_4_6 = (5,0.5)
exponentiation(tuple_4_6)

val tuple_4_7 = (0.5,3)
exponentiation(tuple_4_7)