// Karol Waliszewski

// 1)
def product(a: Double, b: Double): Double =
    a * b

product(1, 2)
product(0, 6)
product(-1.5, 2)
product(2.5, 0.5)

// 2)
def concatenate(list: List[String], endChar: Char, separator: Char):String =
  if(list == Nil)
    ""
  else if(list.tail.isEmpty)
    s"${list.head}$endChar"
  else
    s"${list.head}$separator" + concatenate(list.tail, endChar, separator)

concatenate(List("This","is","test","string"),'.',',');
concatenate(List("Single string"),'.',',');
concatenate(List(),'.',',');

// 3)
def checkIfInRange(list: List[Double], range: (Double,Double)):Boolean =
  if(list.isEmpty)
      throw new Exception("Podano pusta liste.")
  else if(range._1 > range._2)
      throw new Exception("Podano nieprawidłowy zakres.")
  else if(range._1 > list.head || range._2 < list.head)
      false
  else if(!list.tail.isEmpty)
      checkIfInRange(list.tail,range)
  else
      true

checkIfInRange(List(1,2,3),(0,10))
checkIfInRange(List(-1,-2,-3),(0,10))
checkIfInRange(List(0.5),(-1,1))
checkIfInRange(List(-1,1),(-1,1))
checkIfInRange(List(0,0,0),(0,0))
checkIfInRange(List(2,5.5,6),(0,5.5))

// 4)
def power(x: Int, y: Int): Int =
  if(y == 0)
    1
  else if(y == 1)
    x
  else
    x * power(x, y - 1)

power(3,2)
power(2,3)