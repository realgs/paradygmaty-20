package client

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetAddress, Socket}

object Client {
  private var port = 8888
  private var socket: Socket = _
  private var connected = false
  private var waitingForInput = false
  private var eot = false

  def main(args: Array[String]): Unit = {
    if (!args.isEmpty) {
      port = Integer.parseInt(args(0))
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

    val inputThread = new Thread(() => {
      try {
        while (connected) {
          val input = scala.io.StdIn.readInt()
          if (connected && waitingForInput) {
            out.writeInt(input)
            waitingForInput = false
          }
        }
      } catch {
        case e: IOException => e.printStackTrace()
      }
    })
    inputThread.start()

    try {
      while (connected) {
        while (!eot) {
          val messageType = in.readByte()
          messageType match {
            case 0 =>
              connected = false
              waitingForInput = false
              println("Disconnected")
              println("Input any number to quit")
            case 1 =>
              val messageCount = in.readInt()
              Range(0, messageCount).foreach(_ => println(in.readUTF()))
            case 2 => waitingForInput = true
            case 3 => waitingForInput = false
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
      inputThread.interrupt()
    }
  }
}
