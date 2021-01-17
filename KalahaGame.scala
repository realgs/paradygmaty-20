package lab7

import scala.io.StdIn.readInt

class KalahaGame(val numberOfStones: Int)
{
    val numbOfHouses = 6
    val houses: Array[Array[Int]] = Array.ofDim[Int](2,numbOfHouses+1) // Base at index 6
    val baseIdx: Int = 6
    val players : Array[Player] = new Array[Player](2)
    var tokenMove  = 0  // represent who is moving
    var extraMove = false // true if current player has extra move
    var gameON = true
    private def createPlayers(): Unit ={
        for(i<-players.indices)
            players(i) = new CompPlayer(i)
    }
    private def fillBoard(): Unit ={
        for(j<-0 until 2){
            for(i<-0 until numbOfHouses)
            {
                houses(j)(i) = numberOfStones
            }
            houses(j)(baseIdx) = 0
        }
    }
    private def isMovePossible(side: Int): Unit =
    {
        var emptyHouses = 0
        for(i<-0 until numbOfHouses)
        {
            if(houses(side)(i) == 0) emptyHouses += 1
        }
        gameON = emptyHouses != numbOfHouses
    }
    private def takeAllStones(side : Int): Unit =
    {
        for(i<-0 until numbOfHouses){
            houses(side)(baseIdx) += houses(side)(i)
        }
    }
    private def play(): Unit =
    {
        fillBoard()
        while(gameON){
            players(tokenMove).move()
            if(!extraMove)tokenMove = (tokenMove+1)%2
            else extraMove = false
            isMovePossible(tokenMove)
        }
        takeAllStones((tokenMove+1)%2)
        println("Game Over !!! ")
        var result = "Draw"
        if(houses(0)(baseIdx) > houses(1)(baseIdx)) result = "Player 0 won"
        if(houses(0)(baseIdx) < houses(1)(baseIdx)) result = "Player 1 won"
        println("Result "+result+", P0 "+ houses(0)(baseIdx)+", P1 "+ houses(1)(baseIdx))
    }
    def playCC(): Unit =
    {
        createPlayers()
        play()
    }
    def playCH(): Unit =
    {
        println("Hello Player you are playing Kalaha game with computer opponent")
        println("During your turn, enter number of house you would like to move stones from, number between 0-5")
        players(0) = new CompPlayer(0)
        players(1) = new HumanPlayer(1)
        play()
    }
    protected abstract class Player(val side: Int)
    {
        protected val modFactor = numbOfHouses+1
        protected val moveFactor = 1
        def moveStones(houseNumb: Int): Unit =
        {
            var index = houseNumb
            println("Player "+ side+" moved stones from house "+ index)
            val numbOfStones = houses(side)(index)
            houses(side)(index) = 0
            for(i<-1 to numbOfStones)
            {
                index = (index+moveFactor)%modFactor
                houses(side)(index) += 1
                if(i == numbOfStones)
                {
                    if(index == baseIdx){
                        //extra move
                        extraMove = true
                    }
                    else if(houses(side)(index) == 1)
                    {
                        // take enemy stones
                        println(houses((side+1)%2)(numbOfHouses-1-index)+" stones taken from P"+(side+1)%2+" to P"+side)
                        houses(side)(baseIdx) += houses((side+1)%2)(numbOfHouses-1-index) + houses(side)(index)
                        houses((side+1)%2)(numbOfHouses-1-index) = 0
                        houses(side)(index) = 0
                    }
                }
            }
        }
        def printBoard(): Unit=
        {
            println(" Player "+ 0)
            printArray(0)
            println(" Player "+ 1)
            printArray(1)
        }

        def printArray(side: Int): Unit =
        {
            for(i<-0 until numbOfHouses){
                print(houses(side)(i)+", ")
            }
            print(" Base "+houses(side)(baseIdx)+"\n")
        }
        def move(): Unit =
        {
            println("Player " + side + " move")
            moveStones(computeMove())
            printBoard()
            println()
        }
        def computeMove(): Int = 0

    }
    protected class CompPlayer(side: Int) extends Player(side)
    {
        // Strategy
        override def computeMove(): Int =
        {
            def lastStoneIdx(idx: Int): Int =
            {
                //return index where last stone from given house will be put
                ((houses(side)(idx) % modFactor) + idx) % modFactor
            }
            def checkEarnStones(): (List[Int], List[Int] )=
            {
                //check if you can take stones from enemy houses
                //return List of indexes with possible earn in stones
                var result = List[Int]()
                var profit = List[Int]()
                for(i<-0 until numbOfHouses)
                {
                    if(houses((side+1)%2)(numbOfHouses-1-i) != 0)
                    {
                        if(houses(side)(i) == 0) {
                            //check if start from any index end at this empty house
                            for (j <- 0 until numbOfHouses) {
                                if (lastStoneIdx(j) == i && j != i && houses(side)(j) <= numbOfHouses+1){
                                    result = j :: result
                                    profit = houses((side+1)%2)(numbOfHouses-1-i)::profit
                                }
                            }
                        }
                        else{
                            if(lastStoneIdx(i) == i && houses(side)(i) <= numbOfHouses+1){
                                result = i::result
                                profit = houses((side+1)%2)(numbOfHouses-1-i)::profit
                            }
                        }
                    }
                }
                (result,profit)
            }
            def checkExtraMove(): List[Int] ={
                //checks if you can get extra move
                //return List of indexes with possible extra move
                var result = List[Int]()
                for(i<-0 until numbOfHouses)
                {
                    if(lastStoneIdx(i) == baseIdx)result = i::result
                }
                result
            }
            def prepareEmpty(): Int =
            {
                //prepare empty house to take enemy stones in next move
                val canBeEmpty: List[Int] = List[Int]()
                for(i<-1 to numbOfHouses)
                {
                    if(houses(side)(numbOfHouses - i) <= numbOfHouses && houses(side)(numbOfHouses - i) != 0) (numbOfHouses - i)::canBeEmpty
                }
                //simple strategy empty house near enemy base
                if(canBeEmpty.nonEmpty) canBeEmpty.head
                else {
                    var result = 0
                    for(i<-1 to numbOfHouses){
                        if(houses(side)(numbOfHouses-i) != 0 && houses((side+1)%2)(i-1) != 0) result = numbOfHouses-i
                    }
                    result
                }
            }
            def checkBigLost(): (List[Int],List[Int]) =
            {
                //check if enemy can take your big amount of stones from one house
                var dangered: List[Int] = List[Int]()
                var loss: List[Int] = List[Int]()
                for(i<-0 until numbOfHouses)
                {
                    if(houses(side)(i) > numbOfHouses && houses((side+1)%2)(numbOfHouses-1-i) == 0){
                        dangered = i::dangered
                        loss = houses(side)(i)::loss
                    }
                }
                (dangered, loss)
            }
            def getTheBestOne(indexes: List[Int],value: List[Int]): Int =
            {
                var most = 0
                var index = 0
                for(i<-value.indices)
                {
                    if(value.apply(i)>= most) {
                        most = value.apply(i)
                        index = indexes.apply(i)
                    }
                }
                index
            }
            val (earn,profit) = checkEarnStones()
            val (dangered, loss) = checkBigLost()
            val extraMove = checkExtraMove()
            val empty = prepareEmpty()
            if(earn.nonEmpty) getTheBestOne(earn,profit)
            else{
                if(extraMove.nonEmpty) extraMove.head
                else{
                    if(dangered.nonEmpty) getTheBestOne(dangered,loss)
                    else empty
                }
            }
        }
    }
    protected class HumanPlayer(side: Int) extends Player(side)
    {
        private var houseIdx = 0
        override def computeMove(): Int =
        {
            print("Number: ")
            houseIdx = readInt()
            def checkValidIndex(): Unit =
            {
                while(houseIdx > 5 || houseIdx < 0)
                {
                    println("Not valid house index, enter number between 0 and 5")
                    print("Number: ")
                    houseIdx = readInt()
                }
            }
            def checkEmptyHouse(): Unit =
            {
                while(houses(side)(houseIdx) == 0)
                {
                    println("There are no stones in this house, choose another")
                    print("Number: ")
                    houseIdx = readInt()
                    checkValidIndex()
                }
            }
            checkValidIndex()
            checkEmptyHouse()
            houseIdx
        }
    }
}
