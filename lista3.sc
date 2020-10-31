// helper function
def reverse[A](list: List[A]): List[A] = {
  @scala.annotation.tailrec
  def reverseRec(listBase: List[A], reversed: List[A]): List[A] = {
    if (listBase == Nil) reversed
    else reverseRec(listBase.tail, listBase.head :: reversed)
  }
  reverseRec(list, Nil)
}


reverse(List(1, 2, 3)) == List(3, 2, 1)
reverse(List("A", "B", "C")) == List("C", "B", "A")
reverse(Nil) == Nil
reverse(List(1, 1, 1)) == List(1, 1, 1)


// Zadanie 1
def divide(list: List[Int]): (List[Int], List[Int]) = {
  @scala.annotation.tailrec
  def divideRec(list: List[Int], negative: List[Int],
                negative_even: List[Int]): (List[Int], List[Int]) = {
    if (list == Nil) (reverse(negative), reverse(negative_even))
    else if (list.head < 0)
      {
        if (list.head % 2 != 0) divideRec(list.tail, list.head :: negative, list.head :: negative_even)
        else divideRec(list.tail, list.head :: negative, negative_even)
      }
    else divideRec(list.tail, negative, negative_even)
  }
  divideRec(list, Nil, Nil)
}


// testy
divide(List(1, -2, 3, -4, -6, 7, -10)) == (List(-2, -4, -6, -10), Nil)
divide(Nil) == (Nil, Nil)
divide(List(1, 2, 3)) == (Nil, Nil)
divide(List(-1, 2, -3, 4, -5)) == (List(-1, -3, -5), List(-1, -3, -5))
divide(List(1, 2, -0)) == (Nil, Nil)
divide(List(-2, -4, -3)) == (List(-2, -4, -3), List(-3))


// Zadanie 2
def length[A](list: List[A]): Int = {
  @scala.annotation.tailrec
  def lengthRec(list: List[A], n: Int): Int =
    {
        if (list == Nil) n
        else lengthRec(list.tail, n + 1)
    }
  lengthRec(list, 0)
}


// testy
length(List(1, 2, 3, 4, 5, 6)) == 6
length(List(Nil, Nil, Nil)) == 3
length(Nil) == 0
length(List(List(1, 2, 3))) == 1


// Zadanie 3
def concatenate[A](list1: List[A], list2: List[A]): List[A] = {
  @scala.annotation.tailrec
  def mergeRec(list1: List[A], list2: List[A], result: List[A]): List[A] = {
    (list1, list2) match
    {
      case (Nil, Nil) => reverse(result)
      case (Nil, head2 :: tail2) => mergeRec(list1, tail2, head2 :: result)
      case (head1 :: tail1, Nil) => mergeRec(tail1, list2, head1 :: result)
      case (head1 :: tail1, head2 :: tail2) => mergeRec(tail1, tail2, head2 :: head1 :: result)
    }
  }
  mergeRec(list1, list2, Nil)
}


// testy
concatenate(Nil, Nil) == Nil
concatenate(Nil, List(1, 2, 3)) == List(1, 2, 3)
concatenate(List(Nil, Nil), List("A", "B", "C")) == List(Nil, "A", Nil, "B", "C")
concatenate(List(1, 2, 3, 4), List(-1, -2, -3, -4)) == List(1, -1, 2, -2, 3, -3, 4, -4)


// Zadanie 4 a
def comparePatternTail(text: String, pattern: String): Boolean = {
  @scala.annotation.tailrec
  def comparePatternTailRec(compared_text: String, compared_pattern: String, text_accumulator: String): Boolean = {
    if (compared_pattern.isEmpty) true
    else if (compared_text.isEmpty) false
    else if (compared_pattern.head == compared_text.head) comparePatternTailRec(compared_text.tail, compared_pattern.tail, text_accumulator)
    else comparePatternTailRec(text_accumulator.tail, pattern, text_accumulator.tail)
  }
  comparePatternTailRec(text, pattern, text)
}

@scala.annotation.tailrec
def findAny(text: String, phrases: List[String]): Boolean =
{
  if (phrases == Nil) false
  else if (comparePatternTail(text, phrases.head)) true
  else findAny(text, phrases.tail)
}

def findNPhrasesTail(list: List[String], phrases: List[String]): List[String] = {
  @scala.annotation.tailrec
  def findNPhrasesTailRec(list: List[String], phrases: List[String], found: List[String]): List[String] = {
    if (list == Nil) reverse(found)
    else if (findAny(list.head, phrases)) findNPhrasesTailRec(list.tail, phrases, list.head :: found)
    else findNPhrasesTailRec(list.tail, phrases, found)
  }
  findNPhrasesTailRec(list, phrases, Nil)
}

findNPhrasesTail(List("*****", "***"), List("*")) == List("*****", "***")
findNPhrasesTail(List("metallica", "megadeth", "death", "slayer"), List("metal", "death")) == List("metallica", "death")
findNPhrasesTail(List("index0169","index0168202","index0168211", "index0168210","index0169222","index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210")
findNPhrasesTail(Nil, Nil) == Nil
findNPhrasesTail(List("A", "B", "C"), Nil) == Nil
findNPhrasesTail(List("", "", "", ""), List("")) == List("", "", "", "")
findNPhrasesTail(List("", ""), List("Tekst", "tekst")) == Nil
findNPhrasesTail(List("Polska", "Niemcy", "Anglia"), List("")) == List("Polska", "Niemcy", "Anglia")
findNPhrasesTail(List("Klucz", "Wyklucz", "Zaklocenia", "Pokolenia", "ziemniak"), List("lucz", "enia")) == List("Klucz", "Wyklucz", "Zaklocenia", "Pokolenia")


// Zadanie 4 b
def findNPhrases(list: List[String], phrases: List[String]): List[String] = {
  if (list == Nil) Nil
  else if (findAny(list.head, phrases)) list.head :: findNPhrases(list.tail, phrases)
  else findNPhrases(list.tail, phrases)
}


findNPhrases(List("*****", "***"), List("*")) == List("*****", "***")
findNPhrases(List("metallica", "megadeth", "death", "slayer"), List("metal", "death")) == List("metallica", "death")
findNPhrases(List("index0169","index0168202","index0168211", "index0168210","index0169222","index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210")
findNPhrases(Nil, Nil) == Nil
findNPhrases(List("A", "B", "C"), Nil) == Nil
findNPhrases(List("", "", "", ""), List("")) == List("", "", "", "")
findNPhrases(List("", ""), List("Tekst", "tekst")) == Nil
findNPhrases(List("Polska", "Niemcy", "Anglia"), List("")) == List("Polska", "Niemcy", "Anglia")
findNPhrases(List("Klucz", "Wyklucz", "Zaklocenia", "Pokolenia", "ziemniak"), List("lucz", "enia")) == List("Klucz", "Wyklucz", "Zaklocenia", "Pokolenia")



// Zadanie 5 a
def joinTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
  @scala.annotation.tailrec
  def joinRec(list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] = {
    if (list1 != Nil) joinRec(list1.tail, list2, list3, list1.head :: result)
    else if (list2 != Nil) joinRec(list1, list2.tail, list3, list2.head :: result)
    else if (list3 != Nil) joinRec(list1, list2, list3.tail, list3.head :: result)
    else reverse(result)
  }
  joinRec(list1, list2, list3, Nil)
}


// testy
joinTail(List(1, 2), List(3, 4), List(5, 6)) == List(1, 2, 3, 4, 5, 6)
joinTail(List(1, 2), Nil, Nil) == List(1, 2)
joinTail(Nil, Nil, Nil) == Nil
joinTail(Nil, List(1, 2), Nil) == List(1, 2)
joinTail(Nil, Nil, List(1, 2)) == List(1, 2)
joinTail(List("A"), List("B"), List(List(1, 2), List(3, 4))) == List("A", "B", List(1, 2), List(3, 4))


// Zadanie 5 b
def join[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    if (list1 != Nil) list1.head :: join(list1.tail, list2, list3)
    else if (list2 != Nil) list2.head :: join(list1, list2.tail, list3)
    else list3
}


// testy
join(List(1, 2), List(3, 4), List(5, 6)) == List(1, 2, 3, 4, 5, 6)
join(List(1, 2), Nil, Nil) == List(1, 2)
join(Nil, Nil, Nil) == Nil
join(Nil, List(1, 2), Nil) == List(1, 2)
join(Nil, Nil, List(1, 2)) == List(1, 2)
join(List("A"), List("B"), List(List(1, 2), List(3, 4))) == List("A", "B", List(1, 2), List(3, 4))
