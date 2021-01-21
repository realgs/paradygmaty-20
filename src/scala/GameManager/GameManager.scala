// Konrad Karanowski
package GameManager

import akka.actor._

import ClientServer.Server.Play
import ClientServer.{AIPlayer, HumanPlayer, Server}

class GameManager
{
  private[this] val system = ActorSystem("Kalaha")

  private def simulateAIvsAI(): Unit =
  {
    val aiLevel1 = askComputerMode()
    val aiLevel2 = askComputerMode()
    println(aiLevel1, aiLevel2)
    val bottomPlayer = system.actorOf(Props(classOf[AIPlayer], true, aiLevel1))
    val topPlayer = system.actorOf(Props(classOf[AIPlayer], false, aiLevel2))
    val server = system.actorOf(Props(classOf[Server], bottomPlayer, topPlayer))
    server ! Play(true)
  }

  private def playVsAI(): Unit =
  {
    val aiLevel = askComputerMode()
    println(aiLevel)
    val bottomPlayer = system.actorOf(Props(classOf[HumanPlayer], "Bottom"))
    val topPlayer = system.actorOf(Props(classOf[AIPlayer], false, aiLevel))
    val server = system.actorOf(Props(classOf[Server], bottomPlayer, topPlayer))
    server ! Play(true)
  }

  private def playPVP(): Unit =
  {
    val bottomPlayer = system.actorOf(Props(classOf[HumanPlayer], "Bottom"))
    val topPlayer = system.actorOf(Props(classOf[HumanPlayer], "Top"))
    val server = system.actorOf(Props(classOf[Server], bottomPlayer, topPlayer))
    server ! Play(true)
  }

  private def askComputerMode(): Int =
  {
    var response = 0
    while (response < 1 || response > 3)
    {
      println(
        """
          | Choose AI level:
          | 1 - M0R4-V1CK1 -
          |     He makes chaotic, almost random decisions. Beating him shouldn't be a big deal.
          | 2 - 5TU-D3NT -
          |     Do not worry. He is still young, and his decisions are not always the best. You can win this.
          | 3 - D73K-4N -
          |     Do worry. His decisions are very good and deep. This will be a huge challenge.
          |""".stripMargin)
      try response = scala.io.StdIn.readInt()
      catch {case e : Exception => }
    }
    (response - 1) * 6 + 1
  }

  private def askMode(): Int =
  {
    var response = 0
    while (response < 1 || response > 3)
    {
      println(
        """
          | Choose mode:
          | 1 - Play against other player
          | 2 - Play against computer
          | 3 - Simulate computer vs computer
          |""".stripMargin)
      try response = scala.io.StdIn.readInt()
      catch {case _ : Throwable => }
    }
    response
  }

  def run(): Unit =
  {
    println("Welcome to the Kalaha!")
    val responseMode = askMode()
    responseMode match
    {
      case 1 => playPVP()
      case 2 => playVsAI()
      case 3 => simulateAIvsAI()
    }
  }
}

object GameManager
{
  def apply(): GameManager = new GameManager()
}
