object Game {
  def play: Unit = {
    val server = new Server()
    server.start()
  }
}