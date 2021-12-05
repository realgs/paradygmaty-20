import scala.runtime.LazyInt
import scala.runtime.LazyLong
object Tests45 {
    def main(): Unit = {
        println(Tasks45.eachNElement(LazyList.from(0))(5)(24).take(10).toList == List(0,5,10,15,20))
        println(Tasks45.eachNElement(LazyList.from(0))(5)(25).take(10).toList == List(0,5,10,15,20,25))
        println(Tasks45.eachNElement(LazyList.from(0))(1)(24).take(10).toList == List(0,1,2,3,4,5,6,7,8,9))
        println(Tasks45.eachNElement(LazyList.from(0))(1)(8).take(10).toList == List(0,1,2,3,4,5,6,7,8))
        try {Tasks45.eachNElement(LazyList.from(0))(1)(-4)} 
        catch {case e : Throwable => println(e.getMessage() == "m cannot be negative")}

        try {Tasks45.eachNElement(LazyList.from(0))(0)(4)} 
        catch {case e : Throwable => println(e.getMessage() == "n cannot be less than 1")}
        
        val + = (a: Int, b: Int) => a+b
        val - = (a: Int, b: Int) => a-b
        val * = (a: Int, b: Int) => a*b
        val / = (a: Int, b: Int) => a/b

        println(Tasks45.ldzialanie(+)(LazyList.from(1), LazyList.from(2)).take(10).toList == List(3,5,7,9,11,13,15,17,19,21))
        println(Tasks45.ldzialanie(-)(LazyList.from(1), LazyList.from(2)).take(10).toList == List(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1))
        println(Tasks45.ldzialanie(*)(LazyList.from(1), LazyList.from(1)).take(10).toList == List(1,4,9,16,25,36,49,64,81,100))
        println(Tasks45.ldzialanie(/)(LazyList.from(10), LazyList.from(1)).take(10).toList == List(10,5,4,3,2,2,2,2,2,1))
    } 
}
