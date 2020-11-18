class Zad4_5 {
  //Zad4 (5pkt)
  def eachNElement(list: LazyList[Int], n: Int, m: Int): LazyList[Int] = {
    def takeNelement(list: LazyList[Int], index: Int): LazyList[Int] = {
      if (list == LazyList()) list
      else {
        if (index >= m) LazyList()
        else {
          if (index % n == 0) list.head #:: takeNelement(list.tail, index + 1)
          else takeNelement(list.tail, index + 1)
        }
      }
    }

    if (list.size < n) throw new IllegalArgumentException("Index n musi być mniejszy od rozmiaru LazyList")
    if (list.size < m) throw new IllegalArgumentException("Index m musi być mniejszy od rozmiaru LazyList")
    if (m <= 0) throw new IllegalArgumentException("Indeks m jest mniejszy od 1")
    if (n <= 0) throw new IllegalArgumentException("Indeks n jest mniejszy od 1")
    takeNelement(list, 0)
  }
  //Zad5 (5 pkt)
  def ldzialanie(list1: LazyList[Int], list2: LazyList[Int], sep: Char): LazyList[Int] = {
    if (list1 == LazyList() && list2 != LazyList())
      list2.head #:: ldzialanie(list1,list2.tail, sep)

    else if (list1 != LazyList() && list2 == LazyList())
      list1.head #:: ldzialanie(list1.tail,list2, sep)

    else if (list1 == LazyList())
      list1

    else if (list2 == LazyList())
      list2

    else{
      if (sep == '+') {
        (list1.head + list2.head) #:: ldzialanie(list1.tail, list2.tail, sep)
      }
      else if(sep == '-') {
        (list1.head - list2.head) #:: ldzialanie(list1.tail, list2.tail, sep)
      }
      else if(sep == '*') {
        (list1.head * list2.head) #:: ldzialanie(list1.tail, list2.tail, sep)
      }
      else if(sep == '/') {
        (list1.head / list2.head) #:: ldzialanie(list1.tail, list2.tail, sep)
      }
      else{
        throw new IllegalArgumentException("Nie istnieje podanej operacji")
      }
    }
  }
}
val zad_obj = new Zad4_5

zad_obj.eachNElement(LazyList(5,6,3,2,1),2,3).force
zad_obj.eachNElement(LazyList(5,6,3,2,1),2,4).force
zad_obj.eachNElement(LazyList(1,2,3,4,5,6,7,8,9),1,5).force
zad_obj.eachNElement(LazyList(6,5,4,3,2,1),5,1).force


zad_obj.ldzialanie(LazyList(1,2,3,4),LazyList(10,10,10,10,10), '+').force
zad_obj.ldzialanie(LazyList(),LazyList(), '+').force
zad_obj.ldzialanie(LazyList(1,2,3,4),LazyList(), '+').force
zad_obj.ldzialanie(LazyList(),LazyList(10,10,10,10,10), '+').force
zad_obj.ldzialanie(LazyList(15,20,25,45),LazyList(3,2,5), '/').force
zad_obj.ldzialanie(LazyList(3,2,5),LazyList(15,20,25,45), '-').force
zad_obj.ldzialanie(LazyList(15,20,25,45),LazyList(3,2,5), '*').force

//IllegalArgumentException
zad_obj.eachNElement(LazyList(5,6,3,2,1),0,4).force
zad_obj.eachNElement(LazyList(5,6,3,2,1),2,10).force
zad_obj.eachNElement(LazyList(5,6,3,2,1),10,4).force
zad_obj.eachNElement(LazyList(5,6,3,2,1),2,0).force
zad_obj.ldzialanie(LazyList(15,20,25,45),LazyList(3,2,5), 't').force