import l5.{duplicate, duplicateUnique}
import l5_class_examples.{Point, Student}
object l5_tests {
  def main(args: Array[String]): Unit = {

    println(duplicate(List(2, 1, 3, 4, 5), List(2, 0, 4)) == List(2, 2, 3, 3, 3, 3))
    println(duplicate(List(1, 2, 1), List(3, 0, 1, 4)) == List(1, 1, 1, 1))
    println(duplicate(List(), List()) == List())
    println(duplicateUnique(Set(1, 2, 1), List(2, 2, 2, 2, 2)) == List(1, 1, 2, 2))
    println(duplicateUnique(Set(1, 1, 1), List(0, 4)) == List())
    println(duplicateUnique(Set(), List()) == List())
    val p: Point = new Point(2.1, 3.7)
    val s1: Student = new Student("Kowalski", 24, "254545",'M')
    val s2: Student = new Student("Kowalska", 21, "254546",'W')
    p.debugName()
    p.debugVars()
    s1.debugName()
    s1.debugVars()
    s2.debugName()
    s2.debugVars()
    println(p.debugGetName() == "Point")
    println(p.debugGetVars() == List(("x", "double", 2.1), ("y", "double", 3.7)))
    println(s1.debugGetName() == "Student")
    println(s2.debugGetName() == "Student")
    println(s1.debugGetVars() == List(("name", "java.lang.String", "Kowalski"), ("age", "int", 24), ("index", "java.lang.String", "254545"), ("gender", "char", 'M')))
    println(s2.debugGetVars() == List(("name", "java.lang.String", "Kowalska"), ("age", "int", 21), ("index", "java.lang.String", "254546"), ("gender", "char", 'W')))
  }
}
