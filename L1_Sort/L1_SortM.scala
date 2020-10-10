// 1. bubble sort
def bubbleSort(a:Array[Int]):Unit = {
  for(i <- 0 until a.length-1){
    for (j <- 0 until a.length-1-i) {
      if (a(j) > a(j + 1)) {
        val halp = a(j)
        a(j) = a(j + 1)
        a(j + 1) = halp
      }
    }
  }
}

// testing

val testo = Array(9,8,7,6,5,4,3,2,1)
val testo2 = Array(1,2,3,9,8,7,4,6,5)
val testo3 = Array(1,2,3,4,5,6,7,8,9)
val testo4 = Array(9,8,7,1,2,3,6,5,4)

bubbleSort(testo)
testo
bubbleSort(testo2)
testo2
bubbleSort(testo3)
testo3
bubbleSort(testo4)
testo4



// selectsort

def selectSort(a:Array[Int]):Unit = {
  for(i <- 0 until a.length-1) {
    var minimal = i
    for(j <- i+1 until a.length) {
      if(a(j) < a(minimal)) {
        minimal = j
      }
    }
    if(minimal != i) {
      val halp = a(i)
      a(i) = a(minimal)
      a(minimal) = halp
    }
  }
}

// testing 

val testo = Array(9,8,7,6,5,4,3,2,1)
val testo2 = Array(1,2,3,9,8,7,4,6,5)
val testo3 = Array(1,2,3,4,5,6,7,8,9)
val testo4 = Array(9,8,7,1,2,3,6,5,4)

selectSort(testo)
testo
selectSort(testo2)
testo2
selectSort(testo3)
testo3
selectSort(testo4)
testo4

