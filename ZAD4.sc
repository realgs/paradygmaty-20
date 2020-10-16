object ZAD4{
  def main(num:Int,potega:Int): Double ={
    if(potega==0){
      return 1
    }
    else if(potega > 0){
      return num * main(num,potega-1)
    }
    else {
      return 1/main(num,potega+1)
    }
  }
}

val num_1 = 2
val num_2 = 32
val num_3 = 10

print(ZAD4.main(num_1,4))
print(ZAD4.main(num_2,4))
print(ZAD4.main(num_3,4))

