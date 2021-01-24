class Pit(var seeds: Int = 4) {
  def add(): Unit = seeds += 1

  def take(): Int = {
    val returnBalls = seeds
    seeds = 0
    returnBalls
  }

  def get(): Int = seeds

  def copy(): Pit = {
    new Pit(seeds)
  }

  override def toString: String = {
    if (seeds > 9) seeds.toString
    else " " + seeds.toString
  }
}
