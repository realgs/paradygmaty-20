object Zad1 {
  def main(list:List[Int]): Int = {
    if(list != Nil){
      return list.head * main(list.tail)
    }
    else{
      return 1
    }
  }
}

val arr_1 = List(1,2,3,4,5)
val arr_2 = List(-12,423,23,654,12)
val arr_3 = List(-10,-20,30,-40,50, 0)

print(Zad1.main(arr_1))
print(Zad1.main(arr_2))
print(Zad1.main(arr_3))