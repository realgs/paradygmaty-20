package Source.Classes

import Source.Debug

class Dragon(namev: String, colorv: String, agev: Int = 0) extends Debug {

  var name: String = namev
  var color: String = colorv
  var age: Int = agev

  def method(a: Int): Int = a
}
