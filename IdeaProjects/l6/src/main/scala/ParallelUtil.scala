import java.util.concurrent._
import scala.util.DynamicVariable

object ParallelUtil {

  val forkJoinPool=new ForkJoinPool()

  class Scheduler{
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  def scheduler=new DynamicVariable[Scheduler](new Scheduler())

  def parallel[A,B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

}
