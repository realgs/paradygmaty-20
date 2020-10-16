object lista2{
// zad 1
def multiply [A] (lista: List[Double]): Double =
  if (lista.isEmpty) Double.NaN
  else if (lista.length==1) lista.head
  else lista.head*multiply(lista.tail)

// zad 2
def mergeString[A] (lista: List[String],separator: Char,znak: Char): String=
  if (lista.isEmpty) znak.toString
  else if (lista.length==1) lista.head+znak.toString
  else lista.head+separator.toString+mergeString(lista.tail,separator,znak)

// zad 3
def include[A] (lista: List[Double],X:Double,Y:Double): Boolean =
  if (X>Y) false
  else if (lista.isEmpty) false
  else if (lista.size==1 && lista.head>=X && lista.head<=Y) true
  else if (lista.head>=X && lista.head<=Y) include(lista.tail, X, Y)
  else false

// zad 4
def power(podstawa: Double,wykladnik: Int): Double =
  if (wykladnik==0) 1
  else if(wykladnik<0) power(1/podstawa,-wykladnik)
  else if(wykladnik==1) podstawa
  else power(podstawa,wykladnik-1)*podstawa

def main(args: Array[String]): Unit = println("")

println("Zad 1")
println(multiply(List(-1.5,5,0.5,7,0.99)))
println(multiply(List()))
println(multiply(List(4.678)))
println(multiply(List(1.0/0.0,4)))
println(multiply(List(0.0/0.0,4)))

println("\nZad 2")
println(mergeString(List("Ala","ma","kota"),' ','!'))
println(mergeString(List(),' ','!'))
println(mergeString(List("Ala"),' ','!'))
println(mergeString(List("Ala","ma","kota"),'\n','!'))

println("\nZad 3")
println(include(List(2.0,2.5,3.0,4.0),1.0,5.0))
println(include(List(2.0,2.5,3.0,4.0),2.0,4.0))
println(include(List(-2.0,2.5,3.0,9.0),-2.0,4.0))
println(include(List(),2.0,4.0))
println(include(List(2.0,2.5,3.0,4.0),10.0,4.0))

println("\nZad 4")
println(power(2.0,3))
println(power(-2.0,-3))
println(power(2.0,0))
print(power(2.0,1))
}