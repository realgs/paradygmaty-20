//Mateusz Grzesiuk

//Zadanie 1
def p_flatten[A](xs:List[List[A]]): List[A] = {
  if(xs.tail != Nil) xs.head:::p_flatten[A](xs.tail)
  else xs.head
}

def flatten[A](xs:List[List[A]]): List[A] = {
  if(xs == Nil) throw new Exception("pusta lista")
  else p_flatten(xs)
}


flatten(List(List(1,2),List(3,4),List(5,6))) == List(1,2,3,4,5,6)
flatten(List(List(5))) == List(5)
//flatten(List()) // Exception: pusta lista

//Zadanie 2

def p_count[A](x:A, xs:List[A]): Int = {
  if(xs.tail == Nil) {
    if(xs.head == x) 1
    else 0
  }
  else if(xs.head == x) 1 + p_count(x, xs.tail)
  else p_count(x, xs.tail)
}

def count[A](x:A, xs:List[A]): Int = {
  if(xs == Nil) throw new Exception("pusta lista")
  else p_count(x, xs)
}

count('A', List('A','B','A','A','C')) == 3
count(1, List(1)) == 1
count(1, List(0)) == 0
//count('X', List())// Exception: pusta lista

//Zadanie 3

def replicate[A](x:A,i:Int):List[A] = {
  if(i>0) x::replicate(x, i-1)
  else List()
}

replicate("la", 3) == List("la","la","la")
replicate(2, 4) == List(2,2,2,2)
replicate(1,1) == List(1)
replicate(1,0) == Nil

//Zadanie 4

def sqrList(xs:List[Int]): List[Int] = {
  if(xs == Nil) xs
  else (xs.head * xs.head)::sqrList(xs.tail)
}

sqrList(List(1,2,3,4)) == List(1,4,9,16)
sqrList(List(0)) == List(0)
sqrList(List()) == List()

//Zadanie 5

def p_palindrome[A](xs:List[A]): Boolean = {
  if(xs == Nil || xs.tail == Nil) true
  else{
    val xr = xs.tail.reverse
    if(xs.head == xr.head) p_palindrome(xr.tail)
    else false
  }
}

def palindrome[A](xs:List[A]): Boolean = {
  if(xs==Nil) throw new Exception("pusta lista")
  else p_palindrome(xs)
}

palindrome(List(1,2,1))
palindrome(List(1,2,3,4,5,5,4,3,2,1))
palindrome(List(1,2,3,4,5,4,3,2,1))
palindrome(List(1,2,3,4,5))
palindrome(List(1,2,3,4))
!palindrome(List(1, 2, 3, 4, 2, 1))
!palindrome(List(1,2,3,4,4,3,1,1))

//Zadanie 6

def listLength[A](xs:List[A]):Int = {
  if(xs != Nil) 1 + listLength(xs.tail)
  else 0
}

listLength(List(1,2,3)) == 3
listLength(List()) == 0
