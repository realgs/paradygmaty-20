import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class Computer(simulator: Simulator = new Simulator()) extends Players {
  override def move(player: Int): Int = {
    val result = {
      for (i <- 0 until 6)
        yield Future(simulator.simulate(i, player))
    }
    var returnValue = (Int.MinValue, -1)
    for (i <- 0 until 6) {
      val x = Await.result(result(i), 200.seconds)
      if (x > returnValue._1) returnValue = (x, i)
    }
    returnValue._2 + 1
    //returning + 1 because I assume that player choose pit from 1 to 6, and as computer and player
    //are both the engine, than both has to return values between 1 and 6
  }
}
