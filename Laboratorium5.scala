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

  //for test
  val tryingQueue:Queue[Int] = Queue[Int](9,3,7,12);
  val repeatedQueue:Queue[Int] = Queue[Int](1,2,4);
  val tryingStack:Stack[Int] = Stack[Int](1,4);
  val vector:Vector[Int] = Vector[Int](2,3,4);
  val vector2:Vector[Int] = Vector[Int](2,3,4,7);
  val vector3:Vector[Int] = Vector[Int]();
  val repeated:Vector[Int] = Vector[Int](1,2,3);
  val repeated2:Vector[Int] = Vector[Int](1,2,3,2,12);
  val repeated3:Vector[Int] = Vector[Int]();
  val set:Set[Int] = Set[Int](2,5,2,7,9);
  val set2:Set[Int] = Set[Int]();

//Zadanie1 (2.5pkt)
  def duplicate(vector:Vector[Int], repeat:Vector[Int]) : Vector[Int] = {
      def repeated(value:Int, many:Int, exit:Vector[Int]): Vector[Int] ={
        Vector.fill(many)(value)++exit;
      }
    vector match {
      case head+:tail => if(!repeat.isEmpty)repeated(head,repeat.head,duplicate(tail,repeat.tail)) else Vector();
      case _ => Vector();
    }
  }

  println("Test zadanie 1:");
  println("Duplicated = " + duplicate(vector,repeated));
  println("Duplicated = " + duplicate(vector2,repeated2));
  println("Duplicated = " + duplicate(vector2,repeated));
  println("Duplicated = " + duplicate(vector,repeated2));
  println("Duplicated = " + duplicate(vector3,repeated3));
  println("Duplicated = " + duplicate(vector3,repeated));
  println("Duplicated = " + duplicate(vector,repeated3) + "\n");

//zadanie2 (2.5pkt)
  def duplicateNon(set:Set[Int], vector: Vector[Int]):Vector[Int]={
    def repeated(value:Int, many:Int, exit:Vector[Int]): Vector[Int] ={
      Vector.fill(many)(value)++exit;
    }
    set.toVector match {
      case head+:tail => if(!vector.isEmpty)repeated(head,vector.head,duplicateNon(tail.toSet,vector.tail)) else Vector();
      case _ => Vector();
    }
  }

  println("Test zadanie 2:");
  println("DuplicatedNon = " + duplicateNon(set2,repeated2));
  println("DuplicatedNon = " + duplicateNon(set,repeated2) + "\n");

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

    //Zadanie5 (5pkt)
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
    }

  //Classes to tests
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
}
