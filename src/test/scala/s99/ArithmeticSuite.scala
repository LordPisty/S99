package s99

import org.junit.Assert._
import org.junit.{Before, Test}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import s99.Arithmetic._

/**
 * Created by screspi on 9/25/14.
 */
@RunWith(classOf[JUnitRunner])
class ArithmeticSuite extends FunSuite {
  import s99.PLists._

  @Before def initialize() {
  }

  @Test def lastSuccess() {
    assertTrue("7 should be prime", new primeInt(7).isPrime)
  }

  test("gcd check") {
    assert(gcd(36, 63) == 9)
  }

  test("Coprime check") {
    assert(new coprimeInt(35).isCoprimeTo(64))
  }
}