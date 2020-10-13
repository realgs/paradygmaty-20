def pow(x:Double,y:Int):Double =
  if (y == 0) 1
  else if (y < 0 && x != 0) pow(1/x,y*(-1))
  else x * pow(x,y-1)

pow(2,5) == 32
pow(-2,-3) == -0.125
pow(0,0) == 1
pow(2,0) == 1
pow(2.5,3) == 15.625