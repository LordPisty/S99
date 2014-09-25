package s99

/**
 * Created by screspi on 9/25/14.
 */
object PLists {

  // P01 (*) Find the last element of a list.
  //     Example:
  //     scala> last(List(1, 1, 2, 3, 5, 8))
  //     res0: Int = 8
  def last[A](ls: List[A]): A = {
    if (ls.isEmpty) throw new NoSuchElementException
    else if (ls.tail.isEmpty) ls.head else last(ls.tail)
  }

}
