class Board {
  var playerOneHoles=Array(6, 6, 6, 6, 6, 6)
  var playerTwoHoles=Array(6, 6, 6, 6, 6, 6)
  var playerOneBase=0
  var playerTwoBase=0
  var again=false

  def showBoard(): Unit={
    println("PLANSZA:")
    println(playerTwoBase + " | " + playerTwoHoles(5) + " | "+ playerTwoHoles(4) + " | "+ playerTwoHoles(3) + " | "+
      playerTwoHoles(2) + " | "+ playerTwoHoles(1) + " | "+ playerTwoHoles(0) + " | "+" ")
    println("------------------------------")
    println(" " + " | " + playerOneHoles(0) + " | "+ playerOneHoles(1) + " | "+ playerOneHoles(2) + " | "+
      playerOneHoles(3) + " | "+ playerOneHoles(4) + " | "+ playerOneHoles(5) + " | " + playerOneBase)

  }

  def endGame(): Unit={
    var i=0
    while (i < playerOneHoles.length) {
      playerOneBase += playerOneHoles(i)
      i += 1
    }
    i=0
    while (i < playerTwoHoles.length) {
      playerTwoBase += playerTwoHoles(i)
      i += 1
    }
  }

  def copy(): Board={
    val board=new Board
    board.again=again
    board.playerOneBase=playerOneBase
    board.playerTwoBase=playerTwoBase
    board.playerOneHoles=playerOneHoles.clone()
    board.playerTwoHoles=playerTwoHoles.clone()
    board
  }

  def isAvailableForPlayer(i: Int, player: Int): Boolean ={
    if(player==1){
      if(playerOneHoles(i)==0) false else true
    }else{
      if(playerTwoHoles(i)==0) false else true
    }
  }

  def result(): Unit={
    println("Player one - "+playerOneBase+" points")
    println("Player two - "+playerTwoBase+" points")
  }

  def moveFromHole(i: Int, playerOne: Boolean): Unit={
    var turn=playerOne
    var position=i+1
    var counter=(-1)
    if(playerOne){
      counter=playerOneHoles(i)
      playerOneHoles(i)=0
    }else{
      counter=playerTwoHoles(i)
      playerTwoHoles(i)=0
    }
    while(counter!=0){
      if(turn==playerOne){
        if(position==6){
          if(counter==1){
            again=true
          }
          turn=(!turn)
          if(playerOne){
            playerOneBase+=1
          }else{
            playerTwoBase+=1
          }
          counter-=1
          position%=6
          position-=1
        }else{
          if(counter==1){
            if(playerOne && playerOneHoles(position)==0){
              counter-=1
              playerOneBase+=1
              playerOneBase+=playerTwoHoles(5-position)
              playerTwoHoles(5-position)=0
            } else if(!playerOne && playerTwoHoles(position)==0){
              counter-=1
              playerTwoBase+=1
              playerTwoBase+=playerOneHoles(5-position)
              playerOneHoles(5-position)=0
            }
          }else{
            if(turn) playerOneHoles(position)+=1 else playerTwoHoles(position)+=1
            counter-=1
          }
        }
      }else{
        if(position==6){
          turn=(!turn)
          position%=6
          position-=1
        }else{
          if(turn) playerOneHoles(position)+=1 else playerTwoHoles(position)+=1
          counter-=1
        }
      }
      position+=1
    }
  }

  def moveFromHole1(i: Int, playerOne: Boolean): Board={
    var turn=playerOne
    var position=i+1
    var counter=(-1)
    if(playerOne){
      counter=playerOneHoles(i)
      playerOneHoles(i)=0
    }else{
      counter=playerTwoHoles(i)
      playerTwoHoles(i)=0
    }
    while(counter!=0){
      if(turn==playerOne){
        if(position==6){
          if(counter==1){
            again=true
          }
          turn=(!turn)
          if(playerOne){
            playerOneBase+=1
          }else{
            playerTwoBase+=1
          }
          counter-=1
          position%=6
          position-=1
        }else{
          if(counter==1){
            if(playerOne && playerOneHoles(position)==0){
              counter-=1
              playerOneBase+=1
              playerOneBase+=playerTwoHoles(5-position)
              playerTwoHoles(5-position)=0
            } else if(!playerOne && playerTwoHoles(position)==0){
              counter-=1
              playerTwoBase+=1
              playerTwoBase+=playerOneHoles(5-position)
              playerOneHoles(5-position)=0
            }
          }else{
            if(turn) playerOneHoles(position)+=1 else playerTwoHoles(position)+=1
            counter-=1
          }
        }
      }else{
        if(position==6){
          turn=(!turn)
          position%=6
          position-=1
        }else{
          if(turn) playerOneHoles(position)+=1 else playerTwoHoles(position)+=1
          counter-=1
        }
      }
      position+=1
    }
    copy()
  }

}
