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