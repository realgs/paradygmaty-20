
def multiply [A] (lista: List[Double]): Double =
  if (lista.isEmpty) Double.NaN
  else if (lista.length==1) lista.head
  else lista.head*multiply(lista.tail)

multiply(List(-1.5,5,0.5,7,0.99))
multiply(List())
multiply(List(4.678))
multiply(List(1.0/0.0,4))
multiply(List(0.0/0.0,4))

def mergeString[A] (lista: List[String],separator: String,znak: String): String=
  if (lista.isEmpty) znak
  else if (lista.length==1) lista.head+znak
  else lista.head+separator+mergeString(lista.tail,separator,znak)

mergeString(List("Ala","ma","kota")," ","!")