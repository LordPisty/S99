package s99

/**
 * Created by screspi on 9/25/14.
 */
object PLists {

  //    P01 (*) Find the last element of a list.
  //    Example:
  //    scala> last(List(1, 1, 2, 3, 5, 8))
  //    res0: Int = 8
  def last[A](ls: List[A]): A = {
    if (ls.isEmpty) throw new NoSuchElementException
    else if (ls.tail.isEmpty) ls.head else last(ls.tail)
  }

  //    P02 (*) Find the last but one element of a list.
  //    Example:
  //    scala> penultimate(List(1, 1, 2, 3, 5, 8))
  //    res0: Int = 5
  def penultimate[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

  //    P03 (*) Find the Kth element of a list.
  //    By convention, the first element in the list is element 0.
  //    Example:
  //    scala> nth(2, List(1, 1, 2, 3, 5, 8))
  //    res0: Int = 2
  def nth[A](idx: Int, ls: List[A]): A = {
    if (idx == 0) {
      if (!ls.isEmpty) ls.head else throw new NoSuchElementException
    } else {
      if (ls.isEmpty) throw new NoSuchElementException else nth(idx - 1, ls.tail)
    }
  }

  //    P04 (*) Find the number of elements of a list.
  //    Example:
  //    scala> length(List(1, 1, 2, 3, 5, 8))
  //    res0: Int = 6
  def length[A](ls: List[A]): Int = ls match {
    case h :: Nil   => 1
    case _ :: tail  => 1 + length(tail)
    case _          => 0
  }

  //    P05 (*) Reverse a list.
  //    Example:
  //    scala> reverse(List(1, 1, 2, 3, 5, 8))
  //    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[A](ls: List[A]): List[A] = ls match {
    case h :: Nil   => List(h)
    case h :: tail  => reverse(tail) ::: List(h)
    case _          => List()
  }

  //    P06 (*) Find out whether a list is a palindrome.
  //    Example:
  //    scala> isPalindrome(List(1, 2, 3, 2, 1))
  //    res0: Boolean = true
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  def isPalindromeRec[A](ls: List[A]): Boolean = {
    ls.size <=1 || ls.head == ls.last && isPalindromeRec(ls.tail.init)
  }
  //    P07 (**) Flatten a nested list structure.
  //    Example:
  //    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //    res0: List[Any] = List(1, 1, 2, 3, 5, 8)

  //    P08 (**) Eliminate consecutive duplicates of list elements.
  //    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  //    Example:
  //    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
}
