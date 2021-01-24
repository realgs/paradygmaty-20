class Board {
  var playerOneHoles=Array(6, 6, 6, 6, 6, 6)
  var playerTwoHoles=Array(6, 6, 6, 6, 6, 6)
  var playerOneBase=0
  var playerTwoBase=0

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
      if(turn){
        if(position==6){
          turn=false
          playerOneBase+=1
          position%=6
          position-=1
        }else{
          playerOneHoles(position)+=1
        }
      }else{
        if(position==6){
          turn=true
          playerTwoBase+=1
          position%=6
          position-=1
        }else{
          playerTwoHoles(position)+=1
        }
      }
      counter-=1
      position+=1
    }
  }
}
