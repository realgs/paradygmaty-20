class Game(
            p1: Player = new Player("p1", 6, 6),
            p2: Player = new Player("p2", 6, 6)
          ) {
  assert(p1.fieldsNumber == p2.fieldsNumber)
  val numberOfFields:Int = p1.fieldsNumber
  val numberOfPlayers:Int = 2
  var players:Array[Player] = Array(p1,p2)
  var started:Boolean = false

  def getRocksFromOppositeSide(player: Int, field: Int):Unit = {
    players(player).fields(field) = players((player + 1) % numberOfPlayers).fields(numberOfFields - 1 - field)
    players((player + 1) % numberOfPlayers).fields(numberOfFields - 1 - field) = 0
  }

  def move(playerIndex: Int, fieldIndex: Int): Boolean = {
    assert(playerIndex >= 0 && playerIndex < numberOfPlayers && fieldIndex >= 0 && fieldIndex < numberOfFields)

    var repeatMove = false
    var player = players(playerIndex)
    var rocksLeft = player.fields(fieldIndex)
    player.fields(fieldIndex) = 0

    var currentFieldIndex = fieldIndex
    var currentPlayerIndex = playerIndex

    while(rocksLeft > 0) {
      rocksLeft = rocksLeft - 1
      currentFieldIndex = currentFieldIndex + 1

      // Rock in the base
      if(currentFieldIndex == numberOfFields){
        // Last rock is dropped into the base, player gets another move.
        if(rocksLeft == 0){
          repeatMove = true
        }

        player.base = player.base + 1
      }
      // Rock entering other player fields
      else if(currentFieldIndex > numberOfFields) {
        currentFieldIndex = 0;
        currentPlayerIndex = (currentPlayerIndex + 1) % numberOfPlayers
        player = players(currentPlayerIndex)
        player.fields(currentFieldIndex) = player.fields(currentFieldIndex) + 1
      }
      // Standard rock in the field
      else {
        // Last rock is dropped into empty field owned by a player who made a move
        if(rocksLeft == 0 && currentPlayerIndex == playerIndex && player.fields(currentFieldIndex) == 0){
          getRocksFromOppositeSide(currentPlayerIndex, currentFieldIndex)
        }

        player.fields(currentFieldIndex) = player.fields(currentFieldIndex) + 1
      }
    }

    if(checkForEnd){
      finish
    }

    repeatMove
  }

  def calculatePoints(player: Player): Int =
    player.fields.sum + player.base

  def checkForEnd: Boolean =
    calculatePoints(players(0)) == players(0).base || calculatePoints(players(1)) == players(1).base

  def finish: Unit = {
    val p0Points = calculatePoints(players(0))
    val p1Points = calculatePoints(players(1))

    Interface.printFinish

    if(p0Points == p1Points) {
      Interface.printDraw
    }
    else {
      Interface.printWinner(if(p0Points < p1Points) players(1) else players(0))
    }

    Interface.printPoints(players)

    started = false
  }

  def start:Unit = {
    started = true
    Interface.printStartInfo;

    var currentPlayer = players(0)
    var currentPlayerIndex = 0


    while(started) {
      Interface.drawBoard(players)
      Interface.printTurn(currentPlayer)

      val chosenMove = currentPlayer.decideMove
      if(!move(currentPlayerIndex, chosenMove)){
        currentPlayerIndex = (currentPlayerIndex + 1) % numberOfPlayers
        currentPlayer = players(currentPlayerIndex)
      }

    }

  }
}
