import java.util.concurrent.TimeoutException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.io.StdIn.readInt

class Player(exceedTime: Int = 30) extends Engine {

  override def move(player: Int): Int = {
    exceed(exceedTime.seconds, player)
  }

  def exceed(exceedTime: FiniteDuration, player: Int): Int = {
    print("Player" + (player + 1) + " choose pit: ")
    var timedOut = false

    val f1 = Future {
      while (System.in.available() == 0) {
        if (timedOut) throw new TimeoutException()
      }
      val pos = readInt()
      require(pos < 7 && pos > 0)
      pos
    }


    try Await.result(f1, exceedTime)
    catch {
      case _: TimeoutException =>
        println("Sorry, the time is up")
        timedOut = true
        0
      case _: IllegalArgumentException =>
        println("The value must be an integer between 1 and 6")
        -1
    }
  }
}
