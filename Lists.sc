def Product(list:List[Double]):Double={
  if(list.length!=0)
    list.head * Product(list.tail);
  else
      1;
}

Product(List(1,2,3,4));
Product(List(0,3,4,5,2,23,2,4,211,2));
Product(List(-1,2.3,4.5,-5));
Product(List(-4,2));

def AccumulateString(list:List[String],end:Char,separator:Char):String={
    if(list.length==0) return "";
    if(list.length!=1)
        list.head + separator + AccumulateString(list.tail,end,separator);
    else
        list.head+end;
}

AccumulateString(List("abc","def","ghi"),'z','j');
AccumulateString(List(),'j','d');
AccumulateString(List("to","jest","normalne","zdanie"),'.',',');

def CheckNumbers(list:List[Double],x:Double,y:Double):Boolean={
    def check(list:List[Double],x:Double,y:Double):Int={
        if(list.length==0)
            return 0;
        if(list.head<=y && list.head>=x)
            0+check(list.tail,x,y);
        else
            1;
    }

    if(check(list,x,y)==0)
        true;
    else
        false;
}

CheckNumbers(List(1,2,3,4,5),0,6);
CheckNumbers(List(1,2,3,4,5),2,4);
CheckNumbers(List(-1,-0.5,78),-100,200.5);

def Power(x:Double,y:Int):Double={
    if(y==0)
        1;
    else
        x*Power(x,y-1);
}

Power(2,3);
Power(2.5,2);
Power(-3,2);
Power(-2,3);