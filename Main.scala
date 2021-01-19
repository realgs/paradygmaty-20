import akka.actor.{ActorSystem, Props}

object Main
{
  def main(args:Array[String]):Unit =
  {
    startGameAiVsHuman()
  }

  def startGameHumanVsHuman(): Unit =
  {
    val game = Game()

    val ourSystem = ActorSystem("MyActorSystem")
    val server = ourSystem.actorOf(Props(classOf[Server], game), "MyServer")
    val playerOne = ourSystem.actorOf(Props(classOf[HumanPlayer],1,server))
    val playerTwo = ourSystem.actorOf(Props(classOf[HumanPlayer],2,server))

    playerOne ! Start
    playerTwo ! Start

    while(!game.isGameOver()) Thread.sleep(1000)

    ourSystem.terminate
  }

  def startGameHumanVsAi():Unit =
  {
    val game = Game()

    val ourSystem = ActorSystem("MyActorSystem")
    val server = ourSystem.actorOf(Props(classOf[Server], game), "MyServer")
    val playerOne = ourSystem.actorOf(Props(classOf[HumanPlayer],1,server))
    val playerTwo = ourSystem.actorOf(Props(classOf[AIPlayer],2,server,game))

    playerOne ! Start
    playerTwo ! Start

    while(!game.isGameOver()) Thread.sleep(1000)

    ourSystem.terminate
  }

  def startGameAiVsHuman(): Unit =
  {
    val game = Game()

    val ourSystem = ActorSystem("MyActorSystem")
    val server = ourSystem.actorOf(Props(classOf[Server], game), "MyServer")
    val playerOne = ourSystem.actorOf(Props(classOf[AIPlayer],1,server,game))
    val playerTwo = ourSystem.actorOf(Props(classOf[HumanPlayer],2,server))

    playerOne ! Start
    playerTwo ! Start

    while(!game.isGameOver()) Thread.sleep(1000)

    ourSystem.terminate
  }

  def startGameAiVsAi():Unit =
  {
    val game = Game()

    val ourSystem = ActorSystem("MyActorSystem")
    val server = ourSystem.actorOf(Props(classOf[Server], game), "MyServer")
    val playerOne = ourSystem.actorOf(Props(classOf[AIPlayer],1,server,game))
    val playerTwo = ourSystem.actorOf(Props(classOf[AIPlayer],2,server,game))

    playerOne ! Start
    playerTwo ! Start

    while(!game.isGameOver()) Thread.sleep(1000)

    ourSystem.terminate
  }

}
