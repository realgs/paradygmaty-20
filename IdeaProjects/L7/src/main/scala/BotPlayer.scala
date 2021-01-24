import scala.util.Random

class BotPlayer(name: String) extends Player(name: String){

  override def makeDecision(): Int = {
    var r=new Random()
    r.nextInt(6)
  }
}
