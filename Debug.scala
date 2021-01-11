// Zadanie 3 - 5pkt, 4 - 5pkt, 5 - 5pkt
trait Debug {
  def debugName(): String =
  {
    val name = this.getClass.getName
    println(name)
    name
  }

  def debugVars(): Fields =
    {
      val fields = this.getClass.getDeclaredFields
      if (fields.nonEmpty)
        {
          for (field <- fields)
          {
            field.setAccessible(true)
            println("Var: " + field.getName + " => " + field.getAnnotatedType + ", " + field.get(this))
          }
        }
      return new Fields(fields, this)
    }
}
