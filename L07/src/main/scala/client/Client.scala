package client

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetAddress, Socket}

import server.GameState

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Client {
  private var port = 8888
  private var socket: Socket = _
  private var connected = false
  private var waitingForInput = false
  private var eot = false
  private var AIlevel = 4

  def main(args: Array[String]): Unit = {
    if (!args.isEmpty) {
      port = Integer.parseInt(args(0))
    }

    println(menu)

    val option = scala.io.StdIn.readInt()
    val playingAsAI = option == 2
    if (playingAsAI) {
      println("Choose AI difficulty (1/2/3)")
      AIlevel = scala.io.StdIn.readInt() match {
        case 1 => 2
        case 2 => 4
        case 3 => 6
        case _ => 4
      }
    }

    try {
      while (!connected) {
        socket = new Socket(InetAddress.getByName("localhost"), port)
        connected = true
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
        Thread.sleep(1000L)
    }

    val out = new DataOutputStream(socket.getOutputStream)
    val in = new DataInputStream(socket.getInputStream)
    @volatile var currentGameState: GameState = null

    Future {
      try {
        while (connected) {
          if (!playingAsAI) {
            val input = scala.io.StdIn.readInt()
            if (connected && waitingForInput) {
              out.writeInt(input)
              waitingForInput = false
            }
          } else if (currentGameState != null) {
            if (connected && waitingForInput) {
              out.writeInt(AI.getNextMove(currentGameState, AIlevel))
              waitingForInput = false
            }
          }
        }
      } catch {
        case e: IOException => e.printStackTrace()
      }
    }

    try {
      while (connected) {
        while (!eot) {
          val messageType = in.readByte()
          messageType match {
            case 0 =>
              connected = false
              waitingForInput = false
            case 1 =>
              val messageCount = in.readInt()
              Range(0, messageCount).foreach(_ => println(in.readUTF()))
            case 2 => waitingForInput = true
            case 3 =>
              val len = in.readInt()
              currentGameState = GameState.deserialize(in.readNBytes(len))
            case 127 => eot = true
          }
        }

        Thread.sleep(500)
        eot = false
      }
    } catch {
      case e: IOException => e.printStackTrace()
    } finally {
      out.close()
      in.close()
    }
  }

  private def menu: String = {
    """1. Play by yourself
      |2. Play as AI""".stripMargin
  }
}
