//zad 2 - time complex O(n) where n = list.length, space complex O(1)
def length[A] (xs : List[A]) : Int = {
  if (xs == Nil) 0
  else 1 + length(xs.tail)
}
length(List(1,2,3,4))
length(List())