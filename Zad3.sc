object Zad3 {
  def main(list:List[Int], interval:List[Int]): Boolean = {

    if(list != Nil){
      if(list.head < interval.head || list.head > interval.last){
        return false
      }
      else {
        if (list.length ==1){
          return true
        }
        else{
          main(list.tail,interval)
        }
      }
    }
    else {
      return false
    }
  }
}
val interval = List(-40,50)
val arr_1 = List(1,2,3,4,5)
val arr_2 = List(-12,423,23,654,12)
val arr_3 = List(-10,-20,30,-40,50, 0)

print(Zad3.main(arr_1,interval))
print(Zad3.main(arr_2,interval))
print(Zad3.main(arr_3,interval))
