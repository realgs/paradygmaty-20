// Zadanie 5 (5pkt)
trait Debug {
  // Zadanie 3 (5pkt)
  def debugName(): String =
  {
    val name = s"Class: ${getClass.getName}"
    println(name)
    name
  }

  // Zadanie 4 (5pkt)
  def debugVars(): Array[(String, Class[_], AnyRef)] =
  {
    val triples = getClass.getDeclaredFields
      .map(field =>
      {
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this))
      })

    triples.foreach(triple => println(s"Var: ${triple._1} => ${triple._2.getName}, ${triple._3}"))
    triples
  }
}
