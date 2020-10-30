object Tests {
    def main(args: Array[String]): Unit = {
        println(Task1.divide(List(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9)) == (List(-9,-8,-7,-6,-5,-4,-3,-2,-1), List(-9,-7,-5,-3,-1)))
        println(Task1.divide(List(-1, -2, -3, -4, -5, -6)) == (List(-1, -2, -3, -4, -5, -6), List(-1, -3, -5)))
        println(Task1.divide(List(1, 2, 3, 4))  == (List(), List()))
        println(Task1.divide(List()) == (List(), List()))

        println(Task2.count(List()) == 0)
        println(Task2.count(List(1,2,3,4,5,6,7,8,9,0)) == 10)
        println(Task2.count(List("AKA", 'f', 123, 4.567, (1,2), Nil)) == 6)
 
        println(Task3.zip(List(), List()) == List())
        println(Task3.zip(List(1,2,3,4), List("a","b","c","d")) == List(1,"a", 2, "b", 3, "c", 4, "d"))
        println(Task3.zip(List(), List(1,2,3,4)) == List(1,2,3,4))

        println(Task4.findWithTailRec(List("ala", "ma", "kota", "alamakota"), "la") == List("ala", "alamakota"))
        println(Task4.findWithTailRec(List("asd", "aasd", "asasd", "aaaaasssssdddd"), "asd") == List("asd", "aasd", "asasd"))
        
        println(Task4.findWithTailRec(List("ala", "ma", "kota", "alamakota"), List("ka", "ma", "i")) == List("ma", "alamakota"))
        println(Task4.findWithTailRec(List("ala", "ma", "kota", "alamakota"), Nil) == List())

        println(Task4.findWithoutTailRec(List("ala", "ma", "kota", "alamakota"), "la") == List("ala", "alamakota"))
        println(Task4.findWithoutTailRec(List("asd", "aasd", "asasd", "aaaaasssssdddd"), "asd") == List("asd", "aasd", "asasd"))
        
        println(Task4.findWithoutTailRec(List("ala", "ma", "kota", "alamakota"), List("ka", "ma", "i")) == List("ma", "alamakota"))
        println(Task4.findWithoutTailRec(List("ala", "ma", "kota", "alamakota"), Nil) == List())

        println(Task5.join3ListsWithTailRec(List(1,2,"kl"), List(3.45), List(Nil, Nil)) == List(1,2,"kl",3.45, Nil, Nil))
        println(Task5.join3ListsWithTailRec(List(1,2,"kl"), Nil, List(Nil)) == List(1,2,"kl", Nil))
        println(Task5.join3ListsWithTailRec(Nil, Nil, Nil) == Nil)
    
        println(Task5.join3ListsWithoutTailRec(List(1,2,"kl"), List(3.45), List(Nil, Nil)) == List(1,2,"kl",3.45, Nil, Nil))
        println(Task5.join3ListsWithoutTailRec(List(1,2,"kl"), Nil, List(Nil)) == List(1,2,"kl", Nil))
        println(Task5.join3ListsWithoutTailRec(Nil, Nil, Nil) == Nil)
    }       
}
