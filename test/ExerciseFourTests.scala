import org.scalatest.flatspec.AnyFlatSpec
import functions.ExerciseFour._

class ExerciseFourTests extends AnyFlatSpec {
  "Exercise 4 basic function" should "return list of elements which contain given phrase" in {
    assert(find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210"))
    assert(find(List("miasto", "sto", "stoper", "asdas", "wyrazistosc"), "sto") == List("miasto", "sto", "stoper", "wyrazistosc"))
  }

  "Exercise 4 basic function" should "return list of all elements when phrase is \"\"" in {
    assert(find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "") == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
  }

  "Exercise 4 basic function" should "return empty list when list is Nil" in {
    assert(find(Nil, "phrase") == Nil)
  }

  "Exercise 4 basic function with tail recursion" should "return list of elements which contain given phrase" in {
    assert(findWithTailRecursion(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210"))
    assert(find(List("miasto", "sto", "stoper", "asdas", "wyrazistosc"), "sto") == List("miasto", "sto", "stoper", "wyrazistosc"))
  }

  "Exercise 4 basic function with tail recursion" should "return list of all elements when phrase is \"\"" in {
    assert(findWithTailRecursion(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "") == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
  }

  "Exercise 4 basic function with tail recursion" should "return empty list when list is Nil" in {
    assert(findWithTailRecursion(Nil, "phrase") == Nil)
  }

  "Exercise 4 extended function" should "return list of elements which contain given phrase" in {
    assert(findElementsContainingAtLeastOnePhrase(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169")) == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
    assert(findElementsContainingAtLeastOnePhrase(List("miasto", "sto", "stoper", "asdas", "wyrazistosc"), List("sto", "to", "m")) == List("miasto", "sto", "stoper", "wyrazistosc"))
  }

  "Exercise 4 extended function" should "return list of all elements when one of the phrases is \"\"" in {
    assert(findElementsContainingAtLeastOnePhrase(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("blabla", "")) == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
  }

  "Exercise 4 extended function" should "return empty list when list is Nil" in {
    assert(findElementsContainingAtLeastOnePhrase(Nil, List("ph", "ra", "se")) == Nil)
  }

  "Exercise 4 extended function" should "return empty list when there are no phrases" in {
    assert(findElementsContainingAtLeastOnePhrase(List("ph", "ra", "se"), Nil) == Nil)
  }

  "Exercise 4 extended function" should "return Nil where list and phrases are Nil" in {
    assert(findElementsContainingAtLeastOnePhrase(Nil, Nil) == Nil)
  }

  "Exercise 4 extended function with tail recursion" should "return list of elements which contain given phrase" in {
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169")) == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(List("miasto", "sto", "stoper", "asdas", "wyrazistosc"), List("sto", "to", "m")) == List("miasto", "sto", "stoper", "wyrazistosc"))
  }

  "Exercise 4 extended function with tail recursion" should "return list of all elements when one of the phrases is \"\"" in {
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("blabla", "")) == List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"))
  }

  "Exercise 4 extended function with tail recursion" should "return empty list when list is Nil" in {
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(Nil, List("ph", "ra", "se")) == Nil)
  }

  "Exercise 4 extended function with tail recursion" should "return empty list when there are no phrases" in {
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(List("ph", "ra", "se"), Nil) == Nil)
  }

  "Exercise 4 extended function with tail recursion" should "return Nil where list and phrases are Nil" in {
    assert(findElementsContainingAtLeastOnePhraseWithTailRecursion(Nil, Nil) == Nil)
  }

}
