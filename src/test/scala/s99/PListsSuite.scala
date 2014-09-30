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

  test("Flatten should flatten") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  test("Compress success") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Compress empty") {
    assert(compress(List()) == List())
  }

  test("Compress singleton list") {
    assert(compress(List('a)) == List('a))
  }

  test("Compress two list") {
    assert(compress(List('a, 'b)) == List('a, 'b))
  }

  test("Pack") {
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("Pack singleton") {
    assert(pack(List('a)) == List(List('a)))
  }

  test("Pack flat") {
    assert(pack(List('a, 'b, 'c, 'a, 'd, 'e)) == List(List('a), List('b), List('c), List('a), List('d), List('e)))
  }

  test("Encode") {
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Encode singleton") {
    assert(encode(List('a)) == List((1,'a)))
  }

  test("Encode flat") {
    assert(encode(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1,'a), (1,'b), (1,'c), (1,'a), (1,'d), (1,'e)))
  }

  test("Encode Modified") {
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  test("Encode Modified singleton") {
    assert(encodeModified(List('a)) == List('a))
  }

  test("Encode Modified flat") {
    assert(encodeModified(List('a, 'b, 'c, 'a, 'd, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Decode") {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("Direct Encode") {
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Direct Encode singleton") {
    assert(encodeDirect(List('a)) == List((1,'a)))
  }

  test("Direct Encode flat") {
    assert(encodeDirect(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1,'a), (1,'b), (1,'c), (1,'a), (1,'d), (1,'e)))
  }

  test("Duplicate") {
    assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("Duplicate 3") {
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("Duplicate 3 short") {
    assert(duplicateNShort(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("drop") {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("Split") {
    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("Split Functional") {
    assert(splitFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("Split Builtin") {
    assert(splitBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}