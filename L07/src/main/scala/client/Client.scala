package client

import java.io.{DataInputStream, DataOutputStream, IOException}
import java.net.{InetAddress, Socket}

object Client {
  private var port = 8888
  private var socket: Socket = _
  private var connected = false

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

    try {
      while (connected) {
        var eot = false
        var waitingForInput = false

        while (!eot) {
          val messageType = in.readByte()
          messageType match {
            case 0 => connected = false
            case 1 =>
              val messageCount = in.readInt()
              for (_ <- Range(0, messageCount)) {
                println(in.readUTF())
              }
            case 2 => waitingForInput = true
            case 127 => eot = true
          }
        }

        if (connected && waitingForInput) {
          val pit = scala.io.StdIn.readInt()
          out.writeInt(pit)
          waitingForInput = false
        }
      }
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }
}
