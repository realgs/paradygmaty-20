// Zadanie 1
def multiply(list: List[Double]): Double = {
  if (list == Nil) 0
  else if (list.tail == Nil) list.head
  else list.head * multiply(list.tail)
}


// testy Zadania 1
// standard multiplication
multiply(List(1, 2, 3, 4)) // expected -> 24.0
// power
multiply(List(2, 2, 2, 2, 2, 2)) // expected -> 64.0
// negative result
multiply(List(-1, -2, -3, 4)) // expected -> -24.0
// Nil as argument
multiply(List()) // expected -> 0.0
// negative zero
multiply(List(0, 100, -100, 1000, 100, 6.23)) // expected -> -0.0
// small numbers
multiply(List(-0.21, -0.11, 100, -0.1)) // expected -> -0.231


// Zadanie 2
// helper function to represent null as empty string
def add_string(string: String): String = {
  if (string == null) ""
  else string
}


def split(list: List[String], end: String, sep: String): String = {
  // if list is null, return only end
  if (list == Nil) add_string(end)
  // if there is no further elements, return head and end string
  else if (list.tail == Nil) add_string(list.head) + add_string(end)
  // recursive call
  else add_string(list.head) + add_string(sep) + split(list.tail, end, sep)
}


// testy Zadania 2
// standard string
split(List("Jacek", "Sasin", "zmarnowal", "70", "mln", "i", "nie", "poniosl", "odpowiedzialnosci"),
  ".", " ")// expected -> "Jacek Sasin zmarnowal 70 mln i nie poniosl odpowiedzialnsci."
// empty list
split(List(), "!", "") // expected -> "!"
// null end
split(List("Tekst", "String"), null, " SEPARATOR") // expected -> "Tekst SEPARATORString"
// some nulls in list
split(List(null, "To", null), "%", "$") // expected -> "$To$%"
// null separator
split(List("Leo", "Messi"), "", null) // expected -> "LeoMessi"
// some nulls
split(List(null, null), null, null) // expected -> ""


// Zadanie 3
def check_numbers_in_range(list: List[Double], X: Int, Y: Int): Boolean = {
  if ((list.head <= Y) && (list.head >= X))
  {
    if (list.tail == Nil) true
    else check_numbers_in_range(list.tail, X, Y)
  }
  else false
}


def are_numbers_in_range(list: List[Double], X: Int, Y: Int): Boolean = {
  if (X > Y) throw new Exception("Incorrect range, X must be < than Y")
  else if (list == Nil) false
  else check_numbers_in_range(list, X, Y)
}


// Testy zadania 3
// no element in range
are_numbers_in_range(List(1, 2, 3, 4), 6, 7) //  expected -> false
// empty list, not in range
are_numbers_in_range(List(), 0, 10) // expected -> false
// negative X
are_numbers_in_range(List(1, 2, 3, 4), -5, 5) // expected -> true
// all elements are the same
are_numbers_in_range(List(0, 0), 0, 0) // expected -> true
// max(list) > supremum
are_numbers_in_range(List(-7, 7, -7, 7), -10, 5) // expected -> false
// min(list) < infimum
are_numbers_in_range(List(-2.1, 4, 5), -2, 1000) // expected -> false
// X > Y, exception
//are_numbers_in_range(List(1, 2, 3), 2, 1) // expected -> exception


// Zadanie 4
// function to calculate power
def calculate_pow(X: Double, Y: Int): Double = {
  // end of recursion
  if (Y <= 1) X
  // recursive call
  else X * calculate_pow(X, Y - 1)
}


def pow(X: Double, Y: Int): Double = {
  if (X == 0)
    {
      if (Y < 0) throw new Exception("Undefined symbol")
      if (Y == 0) 1
      else 0
    }
  else if ((Y == 0) || (X == 1)) 1
  // for negative Y calculate reciprocal X ^ Y
  else if (Y < 0) 1 / calculate_pow(X, -1 * Y)
  // else calculate power
  else calculate_pow(X, Y)
}


// Testy zadania 4
// standard power
pow(2, 4) // expected -> 16.0
// power with negative X
pow(-3, 7) // expected -> -2187.0
// negative exponent
pow(3, -7) // expected -> 4.57 * 10^-4
// both X and Y negative
pow(-2, -3) // expected -> -1/8
// X to 0
pow(7, 0) // expected -> 1
// undefined symbol exception
pow(0, 0) // expected -> 1
// x e (0, 1)
pow(0.24, 15) // expected -> 5.04 * 10^-10
// big exponent
pow(0, 100000000) // expected -> 0
// negative exponent of 0
pow(0, -3) // expected -> exception
