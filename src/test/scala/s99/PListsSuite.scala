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

  test("Empty list with length 0") {
    assert(length(List()) === 0)
  }

  test("Singleton list with length 1") {
    assert(length(List(342)) === 1)
  }

  test("Long list with length 6") {
    assert(length(List(1,2,3,4,5,6)) === 6)
  }

  test("Reversed string") {
    assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  test("Palindrome check") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
  }

  test("Palindrome Recursive check") {
    assert(isPalindromeRec(List(1, 2, 3, 2, 1)) == true)
  }

  test("Palindrome check false") {
    assert(isPalindrome(List(1, 2, 3, 5, 1)) == false)
  }

  test("Palindrome Recursive check false") {
    assert(isPalindromeRec(List(1, 2)) == false)
  }

}