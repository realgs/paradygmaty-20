import java.util.concurrent.{Callable, TimeUnit, ExecutorService, Future}

// Purely functional parallel computation library
// Created on foundations of Functional Programming in Scala of Manning, Chapter 7

object FPar {
  // FPar abstracts a function:
  type FPar[A] = ExecutorService => Future[A]

  // Unit promotes a computation "a" to FPar and executes it immediately
  def unit[A](a: A): FPar[A] = (es: ExecutorService) => UnitFuture(a)

  // Fork promotes a computation "a" to FPar and marks it for execution later
  def fork[A](a: FPar[A]): FPar[A] = es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  // Async takes a lazily and marks it for later execution
  def async[A](a: => A): FPar[A] = fork(unit(a))

  // UnitFuture wraps around java Future[A]
  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    def isCancelled: Boolean = false

    def isDone: Boolean = true

    def get(timeout: Long, unit: TimeUnit): A = get
  }

  // Map2 maps two concurrent calculations a, b onto concurrent calculation of type C
  // where A, B and C are related by a function f
  def map2[A, B, C](a: FPar[A], b: FPar[B])(f: (A, B) => C): FPar[C] = { (es: ExecutorService) =>
    val af = a(es)
    val bf = b(es)

    UnitFuture(f(af.get, bf.get))
  }

  // Run forces FPar calculation to be evaluated
  def run[A](es: ExecutorService)(a: FPar[A]): A = a(es).get
}
