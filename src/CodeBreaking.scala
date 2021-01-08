import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import Math._

// Purpose of these example is to simulate search of simple code (for example a PIN code) with Brute Force algorithm.
// The code is build from N integer digits, therfore it can be represented as integer number between 0 and 10^N-1
// If code number has less than N digits, then it is assumed it is filed with zeroes from the left side until it has N digits.

class CodeBreaking(val N: Int) {
  assert(N > 0)
  private val secretCode: Int = abs(Random.nextInt(pow(10, N).toInt - 1))
}

object CodeBreaking {

  def bruteForce[A](codeToBreak: CodeBreaking): Int = {
    for (i <- 0 to pow(10, codeToBreak.N).toInt) {
      if (i == codeToBreak.secretCode) {
        return i
      }
    }
    -1
  }

  def bruteForcePar[A](codeToBreak: CodeBreaking): Int = {
    for (i <- (0 to pow(10, codeToBreak.N).toInt).par) {
      if (i == codeToBreak.secretCode) {
        return i
      }
    }
    -1
  }

  def confirmSecretCode(secretCode: CodeBreaking, crackedCode: Int): Boolean = {
    secretCode.secretCode == crackedCode
  }
}
