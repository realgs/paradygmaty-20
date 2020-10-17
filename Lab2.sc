object Lab2 {
 def total(listA:List[Double]):Double =
  {
    def totalRec(listL:List[Double], currentTotal:Double):Double =
      if (listL == Nil) currentTotal
      else totalRec(listL.tail, currentTotal*listL.head)
    if (listA == Nil) 0
    else totalRec(listA, 1.0)
   }                                              //> total: (listA: List[Double])Double
   
   
   total(Nil)                                     //> res0: Double = 0.0
   total(List(3.0, 1.0, 2.0)) == 6.0              //> res1: Boolean = true
   total(List(3.0)) == 3.0                        //> res2: Boolean = true
   total(List(3.0, 1.0, 2.5)) == 7.5              //> res3: Boolean = true
   
   def stringParser(listA:List[String], separator:String, ending:String):String =
   {
    def stringParserRec(listB:List[String], currentString:String, separator:String):String =
      if (listB == Nil) currentString
      else stringParserRec(listB.tail, currentString+separator+listB.head, separator)
      if (listA == Nil) ""
      else stringParserRec(listA.tail, listA.head, separator) + ending
    }                                             //> stringParser: (listA: List[String], separator: String, ending: String)String
                                                  //| 
    
    stringParser(List("Ala","ma","kota"), " ", ".") == "Ala ma kota."
                                                  //> res4: Boolean = true
    stringParser(Nil, " ", ".") == ""             //> res5: Boolean = true
    stringParser(List("la", "aaaaa","ppppa"), "---", "!") == "la---aaaaa---ppppa!"
                                                  //> res6: Boolean = true
    
    
    def isInRange(numbers:List[Double], range:List[Double]):Boolean =
    {
      val lower = range.head
      val upper = range.tail.head
      def isInRangeRec(numbers:List[Double], low:Double, upper:Double):Boolean =
      {
        if (numbers.head < low || numbers.head > upper) false
        else
          if (numbers.tail == Nil) true
          else isInRangeRec(numbers.tail, low, upper)
       }
       if (numbers == Nil) true
       else isInRangeRec(numbers, range.head, range.tail.head)
    }                                             //> isInRange: (numbers: List[Double], range: List[Double])Boolean
    
    
    isInRange(List(2.0, 3.0, 5.0, 3.5), List(3.0, 4.0)) == false
                                                  //> res7: Boolean = true
    isInRange(List(2.0, 3.0, 5.0, 3.5), List(3.0, 5.0)) == false
                                                  //> res8: Boolean = true
    isInRange(List(2.0, 3.0, 5.0, 3.5), List(1.0, 6.0)) == true
                                                  //> res9: Boolean = true
    isInRange(List(2.0, 3.0, 5.0, 3.5), List(2.0, 5.0)) == true
                                                  //> res10: Boolean = true
    isInRange(List(2.5, 3.0, 5.0, 3.5), List(2.0, 5.0)) == true
                                                  //> res11: Boolean = true
    isInRange(Nil, List(2.0,4.0)) == true         //> res12: Boolean = true
  
  def power(base:Double, up:Int):Double =
  {
      def powerRec(base:Double, up:Int, current:Double):Double =
      {
         if (up == 0) current
         else powerRec(base, up-1, current*base)
       }
   if (up ==0) 1
   else powerRec(base, up, 1)
  }                                               //> power: (base: Double, up: Int)Double
  power(3.0, 2) == 9                              //> res13: Boolean = true
  power(3.0, 4) == 81                             //> res14: Boolean = true
  power(3.0, 0) == 1                              //> res15: Boolean = true
  power(4.0, 3) == 64                             //> res16: Boolean = true

}