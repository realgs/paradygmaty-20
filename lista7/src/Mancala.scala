class Mancala(var seeds: Int = 0) {

  def add(): Unit = seeds += 1

  def addAll(newBalls: Int): Unit = seeds += newBalls

  def get(): Int = seeds

  def copy(): Mancala = new Mancala(seeds)

  override def toString: String = seeds.toString
}
