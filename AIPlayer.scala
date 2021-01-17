import akka.actor.{Actor, ActorRef}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.util.Random


class AIPlayer(private val numberOfPlayer:Int,private val server:ActorRef,private val game:Game) extends Actor
{
  implicit var timeout:Timeout = akka.util.Timeout(30.seconds)
  implicit val ex:ExecutionContext = ExecutionContext.global
  final val MAX_DEPTH = 14

  def makeMove(): Int =
  {
    val numberOfZone = minimax(game.copyOfGame(),game.getIndexOfTurnPlayer(),0,-1,(Int.MinValue,-1),(Int.MaxValue,-1))._2
    println(s"AI $numberOfPlayer wybrał numer dołku: $numberOfZone")
    numberOfZone
  }

  private def minimax(gameState:Game,maximizingPlayer:Int,depth:Int,previousMove:Int,alpha:(Int,Int),beta:(Int,Int)):(Int,Int) =
  {
      if(depth == MAX_DEPTH || gameState.isGameOver())
      {
        return (gameState.leadOfPlayer(),previousMove)
      }

      if(maximizingPlayer == numberOfPlayer)
      {
          var newAlpha = alpha
          gameState.validMovesForPlayer().foreach(move => {
            val copyGameState = gameState.copyOfGame()
            copyGameState.move(move)
            val tmpPair = minimax(copyGameState,copyGameState.getIndexOfTurnPlayer(),depth+1,move,newAlpha,beta)
            if(tmpPair._1 > newAlpha._1) newAlpha = (tmpPair._1,move)
            if(newAlpha._1 >= beta._1) return beta

          })
          newAlpha
      }
      else
      {
          var newBeta = beta
          gameState.validMovesForPlayer().foreach(move => {
            val copyGameState = gameState.copyOfGame()
            copyGameState.move(move)
            val tmpPair = minimax(copyGameState,copyGameState.getIndexOfTurnPlayer(),depth+1,move,alpha,newBeta)
            if(tmpPair._1 < newBeta._1) newBeta = (tmpPair._1,move)
            if(alpha._1 >= newBeta._1) return alpha
          })
          newBeta
      }
  }

  override def receive: Receive =
  {
    case Start | CorrectMove => server ! AskForApproval(numberOfPlayer)

    case DoNotGiveApproval => Thread.sleep(500); server ! AskForApproval(numberOfPlayer)

    case GiveApproval => {
      val f = Future{makeMove()}
      try
      {
        val numberOfZone = Await.result(f,timeout.duration)
        server ! MakeMove(numberOfZone)
      }
      catch
        {
          case _:TimeoutException => server ! TimeIsOut
        }
    }

    case GameOver => println(s"AI $numberOfPlayer dziękuję za grę")
  }
}

// Obiekt towarzyszący
object AIPlayer
{
  def apply(numberOfPlayer:Int,server:ActorRef,game:Game):AIPlayer =
  {
    new AIPlayer(numberOfPlayer,server,game)
  }
}
