import java.lang.reflect.{AnnotatedType, Field}

class Fields(fields: Array[Field] = Array(), declaringObject: AnyRef) {
  def getFields: Array[Field] =
    {
      fields
    }

  def getNames: Array[String] =
  {
    for{ field <- fields } yield field.getName
  }

  def getValues: Array[AnyRef] =
    {
      for{ field <- fields } yield
        {
          field.setAccessible(true)
          field.get(declaringObject)
        }
    }

  def getTypes: Array[AnnotatedType] =
    {
      for{ field <- fields} yield field.getAnnotatedType
    }

  override def toString: String =
    {
      val builder = new StringBuilder
      for (field <- fields)
        {
          field.setAccessible(true)
          builder ++= "Var: " + field.getName + " => " + field.getAnnotatedType + ", " + field.get(declaringObject) + "\n"
        }
      builder.toString()
    }
}
