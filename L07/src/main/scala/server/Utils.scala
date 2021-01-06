package server

object Utils {
  def centerString(str: String, size: Int, char: Char): String = {
    val builder = new StringBuilder()

    var i = 0
    while (i < (size - str.length) / 2) {
      builder.append(char)
      i += 1
    }

    builder.append(str)

    while (builder.length() < size) {
      builder.append(char)
    }

    builder.toString
  }
}
