package Board

import actors.Server

class Board
{
  val board: Array[Array[Int]] = Array(Array(6, 6, 6, 6, 6, 6, 0), Array(6, 6, 6, 6, 6, 6, 0))
  val numOfHoles: Int = 6
  val numOfPlayers: Int = 2

  def validateMove(move: Int, player_id: Int): Boolean =
  {
    move >= 0 && move <= (numOfHoles - 1) && board(player_id)(move) != 0
  }

  def makeMovesOnBoard(hole: Int, player_id: Int): Boolean =
  {
    """
      |Method makes moves with stones for a player. Returns true if last stone landed in player's base. False otherwise.
      |""".stripMargin

    val stones = board(player_id)(hole)
    board(player_id)(hole) = 0

    var hole_id = hole
    var row = player_id

    for (_ <- 0 until stones)
    {
      hole_id += 1
      if (hole_id > 6)
      {
        hole_id = 0
        row = (row + 1) % 2
      }
      board(row)(hole_id) += 1
    }

    if (hole_id == 6 && player_id == row)
      true
    else if (player_id == row && board(row)(hole_id) == 1)
    {
      board(player_id)(6) += board((player_id + 1) % 2)(5 - hole_id)
      board((player_id + 1) % 2)(5 - hole_id) = 0
      false
    }
    else false
  }

  def checkGameEnd(board: Board): Int =
  {
    """
      |Checks if either player has no stones in any of his holes. Returns -1 if both players have at least one non-empty hole or
      |id of player that still has stones in any hole.
      |""".stripMargin

    for(i <- 0 until numOfPlayers)
    {
      var hasStones = false
      for (j <- 0 until numOfHoles)
      {
        if (board.board(i)(j) > 0)
          hasStones = true
      }
      if(!hasStones)
        return (i + 1) % 2
    }

    -1
  }

  def gatherPlayersStones(player_id: Int): Unit =
  {
    for (i <- 0 until numOfHoles)
    {
      board(player_id)(6) += board(player_id)(i)
      board(player_id)(i) = 0
    }
  }

  def checkWhoWon(): Int =
  {
    if (board(0)(6) == board(1)(6))
      -1
    else if(board(0)(6) > board(1)(6))
      0
    else
      1
  }

  def findValidMoves(player_id: Int): Seq[Int] =
  {
    var seq = Seq[Int]()
    for (i <- 0 to 5)
      if(board(player_id)(i) > 0)
        seq = seq.appended(i)
    seq
  }

  override def toString: String =
    {
      val builder: StringBuilder = new StringBuilder
      builder.addOne(' ')
      for (i <- 5 to 0 by -1)
        builder.addAll(" " + board(1)(i).toString)
      val firstRowLength = builder.length()
      builder.addOne('\n')
      builder.addAll(board(1)(6).toString)
      for (_ <- 0 until firstRowLength)
        builder.addOne(' ')
      builder.addAll(board(0)(6).toString)
      builder.addOne('\n')
      builder.addOne(' ')
      for (i <- 0 to 5)
        builder.addAll(" " + board(0)(i).toString)

      builder.result()
    }

}
