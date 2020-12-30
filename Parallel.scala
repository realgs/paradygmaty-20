package List6

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}
import scala.util.DynamicVariable

object Parallel {

  private val forkJoinPool = new ForkJoinPool

  private abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
  }

  private class DefaultTaskScheduler extends TaskScheduler {
    override def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case worker: ForkJoinWorkerThread => t.fork()
        case _ => forkJoinPool.execute(t)
      }
      t
    }
  }

  def parallel[A,B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }

  private val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

}
