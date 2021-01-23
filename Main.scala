import akka.actor._
import scala.concurrent.duration._

case class Start()
case class YourMove(var currentBoard:Board)

class Server(val p0:ActorRef,val p1:ActorRef,val board:Board) extends Actor with ActorLogging with Timers{
  var serverBoard:Board = board.copy()

  val timeout:Duration = 30.seconds
  context.setReceiveTimeout(timeout)

  override def receive: Receive = {
    case(Start())=>{
      p0 ! YourMove(serverBoard)
    }
    case (move:Int,0)=>{
      if(move<1 || move>6){
        println("bledny ruch")
        p0 ! YourMove(serverBoard)
      }else{
        val result = serverBoard.move(0,move)
        if(result==0){
          println("gracz 0 ma kolejny ruch")
          p0 ! YourMove(serverBoard)
        }else if(result==1){
          p1!YourMove(serverBoard)
        }else if(result==2){
          serverBoard.printBoard()
          serverBoard.winner()
          context.system.terminate()
        }
      }
    }
    case (move:Int,1)=>{
      if(move<1 || move>6){
        println("bledny ruch")
        p1 ! YourMove(serverBoard)
      }else{
        val result = serverBoard.move(1,move)
        if(result==0){
          println("gracz 1 ma kolejny ruch")
          p1 ! YourMove(serverBoard)
        }else if(result==1){
          p0!YourMove(serverBoard)
        }else if(result==2){
          println("koniec gry, ostateczny stan pola to: ")
          serverBoard.printBoard()
          serverBoard.winner()
          context.system.terminate()
        }
      }
    }
    case ReceiveTimeout=>{
      println("nieaktywnosc gracza , walkowerem wygral komputer")
      println("koniec gry, ostateczny stan pola to: ")
      serverBoard.printBoard()
      context.system.terminate()
    }
  }
}

class Player(val num:Int) extends Actor{
  override def receive: Receive = {
    case YourMove(currentBoard)=>{
      currentBoard.printBoard()
      var move:Int = scala.io.StdIn.readInt()
      while(move>6 || move<1){
        println("bledny ruch")
        move=scala.io.StdIn.readInt()
      }
      if(currentBoard.board(move)==0){
        while(currentBoard.board(move)==0){
          println("bledny ruch")
          move=scala.io.StdIn.readInt()
        }
      }
      sender() ! (move,num)
    }
  }
}

class ArtificialPlayer(val num:Int) extends Actor{
  var board:Board = new Board()
  def bestMove(): (Int,Int) ={
    var advantage:Int=Int.MinValue
    var move:Int=0

    for(x<- 1 to 6){
      val simulationBoard:Board = board.copy()
      if((num==0 && board.board(x)==0) || (num==1 && board.board(x+7)==0)){
        //print("probowal wziac z 0")
      }else {
        simulationBoard.move(num, x)

        for (y <- 1 to 6) {
          val simulationBoard2: Board = simulationBoard.copy()
          var other: Int = 0
          if (num == 0) {
            other = 1
          }
          simulationBoard2.move(other, y)

          if (num == 0 && simulationBoard2.board(7) - simulationBoard2.board(0) > advantage) {
            advantage = simulationBoard2.board(7) - simulationBoard2.board(0)
            move = x
          }
          if (num == 1 && simulationBoard2.board(0) - simulationBoard2.board(7) > advantage) {
            advantage = simulationBoard2.board(0) - simulationBoard2.board(7)
            move = x
          }
        }
      }
    }
    return (move,num)
  }
  override def receive:Receive ={
    case(YourMove(currentBoard))=>{
      currentBoard.printBoard()
      board=currentBoard
      sender() ! bestMove()
    }
  }
}

class Board{
  var board:Array[Int] = Array(0,6,6,6,6,6,6,0,6,6,6,6,6,6)

  def copy():Board={
    val toReturn:Board = new Board()
    val newArray = this.board.clone()
    toReturn.board = newArray
    return toReturn
  }

  def move(player:Int,hole:Int): Int ={
    var index:Int=0;
    if(player==0) {
      index = hole
    }else{
      index = hole+7
    }

    val moves = board(index)//tyle kulek wyjal z dolka
    board(index) = 0
    var i=index+1;//stad zaczyna
    if(i>13) i=0
    var j:Int = 0

    for(x <- 1 to moves){//wkladanie do kolejnych dolkow
      board(i) = board(i)+1
      j=i
      i=i+1
      if(i>13) i=0
    }

    if(board(j)==1 ){//jesli ostatni jest pusty
      if(j>0 && j<7 && player==0){
        board(j)=board(j)+board(j+7)
        board(j+7)=0

      }
      if(j>7 && player==1){
        board(j)=board(j)+board(j-7)
        board(j-7)=0
      }
    }

    if(board(1)+board(2)+board(3)+board(4)+board(5)+board(6)==0 || board(8)+board(9)+board(10)+board(11)+board(12)+board(13)==0){//koniec
      return 2
    }
    if((j==0 && player==1)||(j==7 && player==0)){//gracz ma kolejny ruch
      return 0
    }else{
      return 1//gramy dalej
    }
  }

  def winner(): Unit ={
    if(board(0)>board(7))
      println("wygrał gracz 1")
    else if(board(7)>board(0))
      println("wygrał gracz 0")
    else
      println("remis")
  }

  def printBoard(): Unit ={
    print("  ")
    for(x<- (8 to 13).reverse){
      print(board(x) + " ")
    }
    println()
    println(board(0)+"            "+board(7))
    print("  ")
    for(x<-1 to 6){
      print(board(x) + " ")
    }
    println()
  }
}

object Main extends App{
  var board = new Board()
  var system:ActorSystem = ActorSystem()
  var p0: ActorRef = system.actorOf(Props(classOf[Player], 0))//ArtificialPlayer dla komp vs komp, Player dla gracz vs komp, z zalozenia gracz zawsze jest playerem 0
  var p1: ActorRef = system.actorOf(Props(classOf[ArtificialPlayer], 1))
  var server: ActorRef = system.actorOf(Props(classOf[Server], p0,p1,board))
  server!Start()
  //przy graniu gracz vs komp : przed kazdym ruchem wyswielta sie aktualny stan pola, dolna czesc jest nasza, wpisujemy liczbę od 1 do 6 liczac od lewej sybolizującą z ktorego dolka chcemy wyjac kulki
}
