import java.lang.reflect.{AnnotatedElement, Field}

import scala.collection.{immutable, mutable}
import scala.collection.immutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.LinkedHashSet
import scala.collection.immutable.Seq
import scala.collection.immutable.Vector
import scala.collection.immutable.Set
import scala.collection.immutable.Vector
import scala.collection.immutable.Map

object Lista5 extends App{

  //for tests
  val tryingQueue:Queue[Int] = Queue[Int](9,3,7,12);
  val repeatedQueue:Queue[Int] = Queue[Int](1,2,4);
  val tryingStack:Stack[Int] = Stack[Int](1,4);

  val vector:Vector[Int] = Vector[Int](2,3,4);
  val vector2:Vector[Int] = Vector[Int](2,3,4,7);
  val vector3:Vector[Int] = Vector[Int](2,5,2,7,9);
  val vector4:Vector[Int] = Vector[Int]();

  val vectorString:Vector[String] = Vector[String]("1","3","8");
  val vectorString2:Vector[String] = Vector[String]("4","6","15","6");

  val repeated:Vector[Int] = Vector[Int](1,2,3);
  val repeated2:Vector[Int] = Vector[Int](1,3,4,2,12);
  val repeated3:Vector[Int] = Vector[Int](-1,2);
  val repeated4:Vector[Int] = Vector[Int]();

//Zadanie1 (2.5pkt)
  def duplicate[A](vector:Vector[A], repeat:Vector[Int]) : Vector[A] = {
      def repeated(value:A, many:Int, exit:Vector[A]): Vector[A] ={
        Vector.fill(many)(value)++exit;
      }
    vector match {
      case head+:tail => if(!repeat.isEmpty)repeated(head,repeat.head,duplicate(tail,repeat.tail)) else Vector();
      case _ => Vector();
    }
  }

  println("Test zadanie 1:");
  println(duplicate(vector,repeated) == Vector(2,3,3,4,4,4));//(2,3,4)-(1,2,3)
  println(duplicate(vector2,repeated2) == Vector(2,3,3,3,4,4,4,4,7,7));//(2,3,4,7)-(1,3,4,2,12)
  println(duplicate(vector2,repeated) == Vector(2,3,3,4,4,4));//(2,3,4,7)-(1,2,3)
  println(duplicate(vector3,repeated3) == Vector(5,5));//(2,5,2,7,9)-(-1,2)
  println(duplicate(vectorString,repeated) == Vector("1","3","3","8","8","8"));//("1","3","8")-(1,2,3)
  println(duplicate(vectorString2,repeated4) == Vector());//("4","6","15","6")-()
  println(duplicate(vector4,repeated3) == Vector());//()-(-1,2)
  println();

//zadanie2 (2.5pkt)
  def duplicateNon[A](set:Vector[A], vector: Vector[Int]):Vector[A]={
    def repeated[A](value:A, many:Int, exit:Vector[A]): Vector[A] ={
      Vector.fill(many)(value)++exit;
    }
    val s:Set[A] = set.toSet;
    s.toVector match {
      case head+:tail => if(!vector.isEmpty)repeated(head,vector.head,duplicateNon(tail,vector.tail)) else Vector();
      case _ => Vector();
    }
  }

  println("Test zadanie 2:");
  println(duplicateNon(vector3,repeated2) == Vector(2, 5, 5, 5, 7, 7, 7, 7, 9, 9));//(2,5,2,7,9)-(1,3,4,2,12)
  println(duplicateNon(vector4,repeated2) == Vector());//()-(1,3,4,2,12)
  println(duplicateNon(vectorString2,repeated2) == Vector("4","6","6","6","15","15","15","15"));//("4","6","15","6")-(1,3,4,2,12)
  println();

  //Zadanie3 (5pkt)
    trait Debug{
      def debugName():Unit={
        val str:Array[String] = getClass.toString.split('$');
        println("Class: " + str(1));
      }

    //Zadanie4 (5pkt)
    def debugVars():Unit={
      val field:Array[Field] = getClass.getDeclaredFields;
      for(value <- field) {
        value.setAccessible(true);
        println("Var: " + value.getName + " => " + value.getAnnotatedType + ", " + value.get(this))
      };
    }

    //Zadanie5 (5pkt) // poprawic
      def debugClass(): Array[String] ={
        var className:Array[String] = Array[String]("Class: " +  getClass.getSimpleName);
        val fields:Array[Field] = getClass.getDeclaredFields;
        var allInfo:Array[String] = Array[String]();
        allInfo = className;
          for(value <- fields) {
            value.setAccessible(true);
            allInfo = allInfo:+ ("Var: " + value.getName + " => " + value.getAnnotatedType + ", " + value.get(this))
           };
        allInfo;
      }

    //Przerobiona wersja
    def debugClassO(): Array[Object] = {
      var className: Array[Object] = Array[Object](getClass.getSimpleName);
      val fields: Array[Field] = getClass.getDeclaredFields;
      var allInfo: Array[Object] = Array[Object]();
      allInfo = className;
      for (value <- fields) {
        value.setAccessible(true);
        allInfo = allInfo :+ (value.getName + value.getAnnotatedType + value.get(this))
      };
      allInfo;
    }
    }

  //Classes for tests
    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

  class Student(imie:String, nazwisko:String, wiek:Int) extends Debug{
    var name = imie;
    var surName = nazwisko;
    var age = wiek;
  }

  var p : Point = new Point(3,4);
  var s : Student = new Student("Michal","Kowalski",20);

  println("Test zadanie 3:");
  p.debugName();
  s.debugName();
  println("\n");

  println("Test zadanie 4:");
  p.debugVars();
  s.debugVars()
  println("\n");

  println("Test zadanie 5:");
  println(p.debugClass().toList);
  println(s.debugClass().toList);
  println();
  println(p.debugClassO().toList);
  println(s.debugClassO().toList);
}
