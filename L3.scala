

  object L3 {
    
         def reverse[A] (list : List[A]) : List[A] = {
      def helper (list : List[A] , result : List[A]) : List[A] =
        if (list==Nil) result
        else helper(list.tail,list.head :: result)
      helper(list,Nil)
         }
         
       def  poczatkowySegment (szukanyElement : String , s : String) : Boolean = 
         (szukanyElement,s) match {
         case ("","") => true
         case ("",_) => true
         case (_,"") => false
         case (_,_) => szukanyElement.head==s.head && poczatkowySegment(szukanyElement.tail,s.tail) 
       }
       
       def segment (szukanyElement : String , s : String) : Boolean = 
         if ( s=="") false
         else poczatkowySegment(szukanyElement,s) || segment(szukanyElement,s.tail)
        
    //zadanie 1
    def splitList (list : List[Int]) : (List[Int],List[Int]) =
      list match {
      case Nil => (Nil,Nil)
      case head :: tail => {
        var (list_liczb_ujemnych , list_liczb_ujemnych_nieparzystych) = splitList(tail)
        if (head < 0 && head % 2 == -1)
          (head :: list_liczb_ujemnych , head :: list_liczb_ujemnych_nieparzystych)
          else if (head<0) (head :: list_liczb_ujemnych , list_liczb_ujemnych_nieparzystych)
          else (list_liczb_ujemnych ,list_liczb_ujemnych_nieparzystych)
    }
      }
    
    // zloznosc obliczeniowa to O(n) a zloznosc pameciowa to O(n)
    //zadanie 2 
    def length[A](list : List[A] ) : Int =
      if (list == Nil ) 0
      else 1 + length(list.tail)
      
      //zadanie 2(rekursja ogonowa) , zloznosc obliczeniowa to O(n) a zloznosc pameciowa to O(1)
      def lengthTail[A](list : List[A] ) : Int ={
        def lengthHelper[A](list : List[A],result : Int ) : Int =
          if (list == Nil ) result
          else lengthHelper(list.tail,result+1)

          lengthHelper(list,0)
      }
      
      //zadanie 3
      def polacz [A](list1 : List[A], list2 : List[A] ) : List[A] =
        (list1,list2) match {
        case (Nil,Nil) => Nil
        case (_,Nil) => list1.head :: polacz(list1.tail,Nil)
        case (Nil,_) => list2.head :: polacz(Nil,list2.tail)
        case _ => list1.head :: list2.head :: polacz(list1.tail,list2.tail)
      }
      
      //zadanie 4 rekrusja nieogonowa
      def find (list : List[String],element : String) : List[String] = 
     list match {
        case Nil => Nil
        case head :: tail=> {
        var listStrings = find(list.tail,element)
        if (segment(element,head))  head :: find(tail,element)
        else listStrings
      }
      } 
      
      def findTail (list : List[String],element : String) : List[String] = {
        def helper (list : List[String],element : String,result : List[String]) : List[String] =
          if (list== Nil) result 
          else {
         var  list2= helper(list.tail,element,result)
            if (segment(element,list.head)) helper(list.tail,element,list.head :: result)
            else list2
      }
        reverse(helper(list,element,Nil))
      }   
       
      //zadaniee 5 rekursja nieogonowa
      def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = 
      if (list1 != Nil) list1.head :: joinLists(list1.tail, list2, list3)
      else if (list2 != Nil) list2.head :: joinLists(Nil, list2.tail, list3)
      else if (list3 != Nil) list3.head :: joinLists(Nil, Nil, list3.tail)
      else Nil
      
      //zadaniee 5 rekursja ogonowa
      def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
        def helper[A](list1: List[A], list2: List[A], list3: List[A] , result : List[A]) : List[A] = 
        if (list1 != Nil ) helper(list1.tail ,list2,list3,list1.head :: result)
        else if (list2!= Nil) helper(Nil ,list2.tail,list3,list2.head :: result)
        else if (list3!= Nil) helper(Nil ,Nil,list3.tail,list3.head :: result)
        else result

    reverse(helper(list1,list2,list3,Nil))
      }
        
    def main(args: Array[String]) = {
      println(splitList(List(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10)))
      println(splitList(List()))
      println(splitList(List(-2,-2,-2,-2,-3))+"\n")

      println(length(List(1,2,3,4,5,6,7,8,9)))
      println(length(List("ala","ma","kota")))
      println(lengthTail(List(2,4,6,8,10,12,14,16,18,20))+"\n")
      
      println(polacz(List(1,3,5,7,9,11,13,15,17,19),List(2,4,6,8,10,12,14,16,18,20)))
      println(polacz(List("Ala","kota"),List("ma")))
      println(polacz(List("today","is","the","day"),List())+"\n")
      
      println(joinLists(List(1,2,3,4,5,6,7,8),List(9,10,11,12),List(13,14,15,16,17,18,19,20)))
      println(joinLists(List(1,2,3,4,5,6,7,8),List(),List()))
      println(joinLists(List(1,2,3,4,5,6,7,8,9),List("to","sa","liczby","od ","1 ","do " ,"9"),List(".")))
      
      println(joinListsTail(List(1,2,3),List(4,5,6),List(7,8,9)))
      println(joinListsTail(List(1,2,3),List("jeden","dwa","trzy"),List('j','d','t'))+"\n")

      println(findTail(List("hh","hhh","bgfhh","bhhkk","hjhjhjhjh","h","hh555"),"hh"))
      println(findTail(List("hh","hhh","bgfhh","bhhkk","hjhjhjhjh"),"hh"))
      println(find(List("ala","ma","kota"),""))
      println(find(List("ala","ma","kota")," "))
      println(findTail(List("to","be","or","not","to","be"),"b"))

      println(findTail(List("to","be","or","not","to","be"),"n"))
      println(findTail(List("to","be","or","not","to","be"),"to"))
      println(findTail(List("to","be","or","not","to","be"),""))






      
    
      
    }
    
  }
