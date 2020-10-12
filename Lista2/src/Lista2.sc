//zadanie 1
def product(list: List[Double]): Double =
  if(list.isEmpty) 0
  else if(list.tail!=Nil) list.head * product(list.tail)
  else list.head

product(Nil)==0
product(List(1.7, 2, 5.75, 13.75))==268.8125
product(List(1.27))==1.27
product(List(-8.5, 1.2, 5, 3.4))==(-173.39999999999998) //-173,4: zakres bledu maszyny przy liczeniu

