package L7_main

class Engine() {

  def play_two_players(): Unit ={
    val board: Board = new Board

    board.print_board()
    board.print_board()
    while(!board.is_game_over()._1){
      println("Ruch gracza 1 ...")
      var chosenHole = -1
      var extraMove = false
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla gracza 1")
          board.print_board()
          chosenHole = -1
        }
        while(chosenHole > 6 || chosenHole < 1 || !board.is_legal_move(chosenHole)){
          println("Podaj numer niepustego dołka <1-6>")
          chosenHole = scala.io.StdIn.readInt()
        }
        extraMove = board.make_move(chosenHole)
      }
      while(extraMove)
      board.print_board()

      println("Ruch gracza 2")
      extraMove = false
      chosenHole = -1
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla gracza 2")
          board.print_board()
          chosenHole = -1
        }

        while(chosenHole > 6 || chosenHole < 1 || !board.is_legal_move(chosenHole)){
          println("Podaj numer niepustego dołka <1-6>")
          chosenHole = scala.io.StdIn.readInt()
        }
        extraMove = board.make_move(chosenHole)
      }
      while(extraMove)
      board.print_board()
    }
    println("Wygrał gracz " + board.is_game_over()._2)
  }

  def play_with_ai_as_player_one(): Unit ={
    val board: Board = new Board

    board.print_board()
    while(!board.is_game_over()._1){
      println("Ruch gracza 1 ...")
      var chosenHole = -1
      var extraMove = false
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla komputera")
          board.print_board()
          chosenHole = -1
        }
        chosenHole = KalahaAI.chooseMove(board,13, 1)
        println("Komputer wybrał dołek " + chosenHole)

        extraMove = board.make_move(chosenHole)
        if(board.is_game_over()._1){
          if(board.is_game_over()._2 == 0){
            println("Remis")
          }
          else{
            println("Wygrał gracz " + board.is_game_over()._2)
            return
          }
        }
      }
      while(extraMove)
      board.print_board()

      println("Ruch gracza 2")
      extraMove = false
      chosenHole = -1
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla gracza 2")
          board.print_board()
          chosenHole = -1
        }

        while(chosenHole > 6 || chosenHole < 1  || !board.is_legal_move(chosenHole)){
          println("Podaj numer niepustego dołka <1-6>")
          chosenHole = scala.io.StdIn.readInt()
        }
        extraMove = board.make_move(chosenHole)
        if(board.is_game_over()._1){
          if(board.is_game_over()._2 == 0){
            println("Remis")
          }
          else{
            println("Wygrał gracz " + board.is_game_over()._2)
            return
          }
        }
      }
      while(extraMove)
      board.print_board()
    }
    if(board.is_game_over()._2 == 0){
      println("Remis")
    }
    else{
      println("Wygrał gracz " + board.is_game_over()._2)
    }
    board.print_board()

  }

  def play_with_ai_as_player_two(): Unit ={
    val board: Board = new Board

    board.print_board()
    while(!board.is_game_over()._1){
      println("Ruch gracza 1 ...")
      var chosenHole = -1
      var extraMove = false
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla gracza 1")
          board.print_board()
          chosenHole = -1
        }
        while(chosenHole > 6 || chosenHole < 1  || !board.is_legal_move(chosenHole)){
          println("Podaj numer niepustego dołka <1-6>")
          chosenHole = scala.io.StdIn.readInt()
        }
        extraMove = board.make_move(chosenHole)
        if(board.is_game_over()._1){
          if(board.is_game_over()._2 == 0){
            println("Remis")
          }
          else{
            println("Wygrał gracz " + board.is_game_over()._2)
            return
          }

        }
      }
      while(extraMove)
      board.print_board()

      println("Ruch gracza 2")
      extraMove = false
      chosenHole = -1
      do{
        if(extraMove) {
          println("Ostatnia kulka we własnej bazie, dodatkowy ruch dla komputera")
          board.print_board()
          chosenHole = -1
        }

        chosenHole = KalahaAI.chooseMove(board,13, 2)
        println("Komputer wybrał dołek " + chosenHole)

        extraMove = board.make_move(chosenHole)
        if(board.is_game_over()._1){
          if(board.is_game_over()._2 == 0){
            println("Remis")
          }
          else{
            println("Wygrał gracz " + board.is_game_over()._2)
            return
          }
        }
      }
      while(extraMove)
      board.print_board()
    }
    if(board.is_game_over()._2 == 0){
      println("Remis")
    }
    else{
      println("Wygrał gracz " + board.is_game_over()._2)
    }
    board.print_board()
  }
}
