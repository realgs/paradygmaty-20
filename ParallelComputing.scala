package L6
import java.util.concurrent.{ForkJoinPool, ForkJoinWorkerThread, RecursiveTask}
import java.lang.Thread.currentThread

object ParallelComputing {
  private val fjp = new ForkJoinPool()
  private val scheduler = new TasksScheduler()

  private def task [T](body: => T): RecursiveTask[T] =
    scheduler.schedule(body)

  def parallel [A, B](taskA: => A, taskB: => B): (A, B) =
    scheduler.parallel(taskA, taskB)

  private class TasksScheduler {
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }

    def schedule[T](bodyOfCurrentTask: => T): RecursiveTask[T] = {
      val task = new RecursiveTask[T] {
        def compute: T = bodyOfCurrentTask
      }
      currentThread match {
        case _: ForkJoinWorkerThread => task.fork()
        case _ => fjp.execute(task)
      }
      task
    }
  }
}

