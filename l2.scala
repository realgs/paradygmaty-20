

object l2 {
  


  def iloczyn(list : List[Double ]) : Double = {
    
    
    if (list==Nil)1   
    else 
    {
    
     list.head * iloczyn(list.tail)
    
    
  }
  }
  
def lacz_tekst (list : List[String] , separator : String, znak_konczacy : String) : String = {
  
  if (list == Nil) "" 
  
  else {
   
  
   list.head +separator+ lacz_tekst(list.tail,separator,"") +  znak_konczacy


  }
  
 
}
  
 def liczba_nalezy_do_przedzialu (liczba : Double , x : Double , y : Double) : Boolean ={ 
   
   //przedzial musi miec forme [min,max]
   
   val min= x.min(y)
      val max= x.max(y)

   
   ((liczba>=min && liczba<max) || (liczba>min && liczba<=max)) 
     
  
   
 }
 

   def potega(podstawa : Double , wykladnik : Double) : Double = {
     
     scala.math.pow(podstawa, wykladnik)
     
   }
 
 
 
 
 
   def main(args: Array[String]) = {
     
        println( iloczyn(List(1,2,3,4,5)))
         println( iloczyn(List(2,2,4,4,4)))
          println( iloczyn(List(1.5,4,3,10,2))+"\n")
          
          
        println(lacz_tekst(List("Czesc","politechniko","bardzo","Cie","kocham")," ","."))
        println(lacz_tekst(List("insanity","is","doing","the","exact","thing","expecting","things","to","change"),"    ","."))
        println(lacz_tekst(List("wyglada","na","to","ze","funckja","jakos","dziala","a","nie")," ","??")+"\n")

        println(liczba_nalezy_do_przedzialu(1,3,1))
        println(liczba_nalezy_do_przedzialu(7.5,3,18))
        println(liczba_nalezy_do_przedzialu(-5,-7,-6)+"\n")

        println(potega(5,3))
        println(potega(2,8))
        println(potega(4,3)+"\n")
    }
}