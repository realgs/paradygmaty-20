import java.util.concurrent._

object ParallelUtil {

  val forkJoinPool=ForkJoinPool.commonPool()

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = schedule {
      taskB
    }
    val left = taskA
    (left, right.join())
  }

  def schedule[T](body: => T): ForkJoinTask[T] = {
    val t = new RecursiveTask[T] {
      def compute: T = body
    }
    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork() //dividing into subtasks
      case _ =>
        forkJoinPool.execute(t) //arranging for (asynchronous) execution of the given task.
    }
    t
  }

  def task[T](body: => T): ForkJoinTask[T] = {
    schedule(body)
  }

}
