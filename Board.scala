package L7_main

import scala.collection.mutable

class Board() {
  val DEFAULT_MARBLES_NUMBER = 6;
  val DEFAULT_ROW_SIZE = 6;
  var currentPlayer = 1

  // 1.base 1.holes 2.base 2.holes
  var board_array = Array.fill[Int](DEFAULT_ROW_SIZE*2 + 2)(DEFAULT_MARBLES_NUMBER)
  var last_move_queue = mutable.Queue[Int]()

  // setting bases marbles to 0
  board_array(0) = 0
  board_array(7) = 0

  val PLAYER_1_START_POS = 0
  val PLAYER_2_START_POS = 7
  val BOARD_ARR_LEN = 14

  def make_move(holeNumber: Int): Boolean = {
    var pickHoleIndex = -1
    if (currentPlayer == 1) {
      pickHoleIndex = PLAYER_1_START_POS + holeNumber
    }
    else {
      pickHoleIndex = PLAYER_2_START_POS + holeNumber
    }

    var seedsNumber: Int = board_array(pickHoleIndex)
    board_array(pickHoleIndex) = 0

    var i = pickHoleIndex
    var incr = true
    if(currentPlayer == 2){
      incr = true
    }
    else{
      incr = false
    }

    last_move_queue.clear()
    while (seedsNumber != 0) {

      if (i == 0) {
        i = 8
        incr = true
      } else if(i == 13) {
        i = 7
        incr = false
      }
      else if(incr)
        i += 1
      else
        i -= 1

      if (!((i == 0 && currentPlayer == 2) || (i == 7 && currentPlayer == 1))) { // avoiding enemies bases
        board_array(i) += 1
        seedsNumber -= 1
        last_move_queue.enqueue(i)
      }
    }

    if(currentPlayer == 1 && (i <= 6 && i >= 1) && board_array(i) == 1 && board_array(i + 7) != 0){ // hit
      board_array(0) += (board_array(i + 7) + 1) // moving enemy's marbles to player's base
      board_array(i + 7) = 0
      board_array(i) = 0
    }
    else if(currentPlayer == 2 && (i <= 13 && i >= 8) && board_array(i) == 1 && board_array(i - 7) != 0){ // hit
      board_array(7) += (board_array(i - 7) + 1) // moving enemy's marbles to player's base
      board_array(i - 7) = 0
      board_array(i) = 0
    }
    else if((currentPlayer == 1 && i == 0) || (currentPlayer == 2 && i == 7)){ // one more move for the player
      return true
    }
    next_player
    false
  }

  // only to test if a move returns true or false
  def make_move_without_effects(holeNumber: Int): Boolean = {
    val boardCopy = board_array.clone()

    var pickHoleIndex = -1
    if (currentPlayer == 1) {
      pickHoleIndex = PLAYER_1_START_POS + holeNumber
    }
    else {
      pickHoleIndex = PLAYER_2_START_POS + holeNumber
    }

    var seedsNumber: Int = board_array(pickHoleIndex)
    board_array(pickHoleIndex) = 0

    var i = pickHoleIndex
    var incr = true
    if(currentPlayer == 2){
      incr = true
    }
    else{
      incr = false
    }

    while (seedsNumber != 0) {

      if (i == 0) {
        i = 8
        incr = true
      } else if(i == 13) {
        i = 7
        incr = false
      }
      else if(incr)
        i += 1
      else
        i -= 1

      if (!((i == 0 && currentPlayer == 2) || (i == 7 && currentPlayer == 1))) { // avoiding enemies bases
        board_array(i) += 1
        seedsNumber -= 1
      }
    }

    if(currentPlayer == 1 && (i <= 6 && i >= 1) && board_array(i) == 1 && board_array(i + 7) != 0){ // hit
      board_array(0) += (board_array(i + 7) + 1) // moving enemy's marbles to player's base
      board_array(i + 7) = 0
      board_array(i) = 0
    }
    else if(currentPlayer == 2 && (i <= 13 && i >= 8) && board_array(i) == 1 && board_array(i - 7) != 0){ // hit
      board_array(7) += (board_array(i - 7) + 1) // moving enemy's marbles to player's base
      board_array(i - 7) = 0
      board_array(i) = 0
    }
    else if((currentPlayer == 1 && i == 0) || (currentPlayer == 2 && i == 7)){ // one more move for the player
      board_array = boardCopy
      return true
    }
    board_array = boardCopy
    false
  }

  def is_legal_move(holeNumber: Int): Boolean ={
    if(currentPlayer == 1 && board_array(holeNumber) == 0)
      false
    else if(currentPlayer == 2 && board_array(holeNumber + 7) == 0)
      false
    else
      true
  }

  def print_board(reversed: Boolean = false): Unit={
    print(Console.WHITE + "1:      ")
    for(i <- 1 to 6){
      if(i != 6) {
        if(last_move_queue.contains(i)) {
          print(Console.RED + "[ " + board_array(i) + " ] - ")
        }
        else{
          print(Console.WHITE + "[ " + board_array(i) + " ] - ")
        }
      } else {
        if(last_move_queue.contains(i)) {
          print(Console.RED + "[ " + board_array(i) + " ]\n")

        }
        else{
          print(Console.WHITE + "[ " + board_array(i) + " ]\n")
        }
      }
    }
    if(last_move_queue.contains(0)) {
      print(Console.RED + "   [ " + board_array(0) + " ]")
    }
    else{
      print(Console.WHITE + "   [ " + board_array(0) + " ]")
    }

    for(i <- 1 to 45){
      print(Console.WHITE + " ")
    }
    if(last_move_queue.contains(7)) {
      print(Console.RED + "[ " + board_array(7) + " ]\n")
    }
    else{
      print(Console.WHITE + "[ " + board_array(7) + " ]\n")
    }


    print(Console.WHITE + "2:      ")
    for(i <- 8 to 13){
      if(i != 13) {
        if(last_move_queue.contains(i)) {
          print(Console.RED + "[ " + board_array(i) + " ] - ")
        }
        else{
          print(Console.WHITE + "[ " + board_array(i) + " ] - ")
        }
      } else {
        if(last_move_queue.contains(i)) {
          print(Console.RED + "[ " + board_array(i) + " ]")

        }
        else{
          print(Console.WHITE + "[ " + board_array(i) + " ]\n")
        }
      }
    }
    print(Console.WHITE + "\n")
  }

  def next_player(): Unit ={
    if(currentPlayer == 1){
      currentPlayer = 2
    }
    else{
      currentPlayer = 1
    }
  }

  def is_game_over(): (Boolean, Int) ={
    if(board_array(0) > DEFAULT_MARBLES_NUMBER * 6){
      return (true, 1)
    }
    else if(board_array(7) > DEFAULT_MARBLES_NUMBER * 6){
      return(true, 2)
    }

    var player1RowSum = 0
    for(i <- 1 to 6){
      player1RowSum += board_array(i)
    }
    var player2RowSum = 0
    for(i <- 8 to 13){
      player2RowSum += board_array(i)
    }

    if((player1RowSum == 0 || player2RowSum == 0) && board_array(0) > board_array(7)){
      (true, 1)
    }
    else if((player1RowSum == 0 || player2RowSum == 0) && board_array(0) < board_array(7)){
      (true, 2)
    }
    else if((player1RowSum == 0 || player2RowSum == 0) && board_array(0) == board_array(7)) {
      (true, 0)
    }
    else{
      (false, 0)
    }
  }

  def move_repeat(holeNumber: Int): Boolean ={
    var pickHoleIndex = -1
    if (currentPlayer == 1) {
      pickHoleIndex = PLAYER_1_START_POS + holeNumber
    }
    else {
      pickHoleIndex = PLAYER_2_START_POS + holeNumber
    }
    var seedsNumber: Int = board_array(pickHoleIndex)

    var i = pickHoleIndex
    var incr = true
    if(currentPlayer == 2){
      incr = true
    }
    else{
      incr = false
    }

    while (seedsNumber != 0) {

      if (i == 0) {
        i = 8
        incr = true
      } else if(i == 13) {
        i = 7
        incr = false
      }
      else if(incr)
        i += 1
      else
        i -= 1

      if (!((i == 0 && currentPlayer == 2) || (i == 7 && currentPlayer == 1))) { // avoiding enemies bases
        seedsNumber -= 1
      }
    }
    if((currentPlayer == 1 && i == 0) || (currentPlayer == 2 && i == 7)){ // one more move for the player
      return true
    }

    false
  }

  override def clone(): Board = {
    val board_clone = new Board()
    board_clone.board_array = this.board_array.clone()
    board_clone.currentPlayer = this.currentPlayer

    board_clone
  }
}
