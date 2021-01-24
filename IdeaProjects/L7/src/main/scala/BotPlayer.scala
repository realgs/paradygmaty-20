import scala.util.Random

class BotPlayer(name: String) extends Player(name: String){

  override def makeDecision(): Int = {
    (new Random()).nextInt(6)
  }

//  def createTree(n: Int): BT={
//    def inner(a: Int, boa: Board, choice: List[Int]): BT={
//      var inThat=false
//      if(a==0){
//        Empty
//      }else{
//        if(boa.playerOneBase>biggest){
//          if(choice.size!=0){
//            chosen=choice.reverse.head
//          }
//          biggest=boa.playerOneBase
//        }
//        if((n-a)%2==0){
//          (Node(b, Array(inner(a-1, b.moveFromHole1(0, true), 0::choice), inner(a-1, b.moveFromHole1(1, true), 1::choice),
//            inner(a-1, b.moveFromHole1(2, true), 2::choice), inner(a-1, b.moveFromHole1(3, true), 3::choice),
//            inner(a-1, b.moveFromHole1(4, true), 4::choice), inner(a-1, b.moveFromHole1(5, true), 5::choice))))
//        }else{
//          (Node(b, Array(inner(a-1, b.moveFromHole1(0, false), 0::choice), inner(a-1, b.moveFromHole1(1, false), 1::choice),
//            inner(a-1, b.moveFromHole1(2, false), 2::choice), inner(a-1, b.moveFromHole1(3, false), 3::choice),
//            inner(a-1, b.moveFromHole1(4, false), 4::choice), inner(a-1, b.moveFromHole1(5, false), 5::choice))))
//        }
//      }
//    }
//    inner(n, b, List())
//  }

}
