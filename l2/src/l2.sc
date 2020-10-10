
//Zadanie 1
def product(xs : List[Double]):Double = {
  var ret_val :Double = 1.0
  if(xs.nonEmpty){
    ret_val = xs.head*product(xs.tail)
    ret_val
  }else ret_val
}
product(List(1.0,2.0,3.0)) == 6.0
product(List(1.5,2.0,3.5,6.0)) == 63.0
product(List(1.0,1.0,1.0,-1.0,1.0)) == -1.0
product(List(6.0,8.0,1.0,-1.0,1.0)) == -48.0
product(List(5.0,-1.7,3.9)) == -33.15
product(List()) == 0.0

//Zadanie 2
def combineWords(xs : List[String],last_Sign : String,
                 separator:String ): String = {
  var ret_val: String = ""
  if(xs.nonEmpty) {
    ret_val = ret_val + separator + xs.head
    combineWords(xs.tail, last_Sign, separator)
    ret_val
  }else ret_val+last_Sign
}
print(combineWords(List("Ala","ma","kota"),"."," "))

//Zadanie 3
def checkNumbers(xs : List[Double], X : Double, Y : Double):Boolean = {
   if(xs.nonEmpty){
     if(xs.head>=X && xs.head<=Y) checkNumbers(xs.tail,X,Y)
     else false
   } else true
}

checkNumbers(List(1.0,2.0,3.0),1.0,3.0) == true
checkNumbers(List(-5.8,1.0,2.0,3.0,40.7),-6.0,10.0) == false
checkNumbers(List(),1.0,3.0) == true
checkNumbers(List(4.0,7.0,38.0,-3.6),-4.0,40.5) == true
checkNumbers(List(0.0, 3.5, 0.00001),1.0,3.0) == false

//Zadanie 4
def toPower(base : Double, exponent : Int):Double ={
    if(exponent > 0){
     base*toPower(base,exponent-1)

    } else if(exponent < 0) {
      (1/base)*toPower(base,exponent+1)

    }else 1.0

}
toPower(2.0,0) == 1.0
toPower(2.0,3) == 8.0
toPower(2.0,-2) == 0.25
toPower(2.0,10) == 1024.0
toPower(1.0,1000) == 1.0