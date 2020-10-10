val rand = scala.util.Random

def quicksort (array: Array[Int]): Array[Int] =
{
  if (array.length < 2) array
  else
  {
    val pivot = array(rand.nextInt(array.length))
    Array.concat(
      quicksort(array filter pivot.>),
      array filter pivot.==,
      quicksort(array filter pivot.<)
    )
  }
}