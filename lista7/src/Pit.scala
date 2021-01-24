class Pit(var seeds: Int = 4) {
  def add(): Unit = seeds += 1

  def get(): Int = seeds

  def copy(): Pit = new Pit(seeds)

  def take(): Int = {
    val returnBalls = seeds
    seeds = 0
    returnBalls
  }

  override def toString: String = {
    if (seeds > 9) seeds.toString
    else " " + seeds.toString
  }
}
