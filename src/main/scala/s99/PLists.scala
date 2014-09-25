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
}
