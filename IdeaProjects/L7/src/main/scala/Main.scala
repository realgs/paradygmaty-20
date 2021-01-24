import akka.actor.{ActorSystem, Props}

object Main extends App{

  val board = new Board
  board.showBoard()

  val mySystem=ActorSystem("mySystem")
  val player1=mySystem.actorOf(Props(new BotPlayer("player1")))
  val player2=mySystem.actorOf(Props(new BotPlayer("player2")))
  val server=mySystem.actorOf(Props(new Server(player1, player2)))
  server ! Start
  mySystem.terminate()
}
