package s99

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

/**
 * Created by screspi on 9/25/14.
 */
@RunWith(classOf[JUnitRunner])
class PListsSuite extends FunSuite {
  import PLists._

  @Before def initialize() {
  }

  @Test def lastSuccess() {
    assertTrue("The last element should be the last element", last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  test("Last on empty list should throw") {
    intercept[NoSuchElementException] {last(List[Int]())}
  }

  test("Penultimate should return proper element") {
    assert(penultimate(List(1,1,2,3,5,8)) === 5)
  }

  test("Second element should return - zero based index") {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }
}