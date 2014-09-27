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
  def flatten(ls: List[Any]): List[Any] = ls match {
    case h :: Nil => h match {
      case p: List[Any] => flatten(p)
      case x => List(x)
    }
    case h :: tail => h match {
      case p: List[Any] => flatten(p) ::: flatten(tail)
      case x => List(x) ::: flatten(tail)
    }
  }

  //    P08 (**) Eliminate consecutive duplicates of list elements.
  //    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  //    Example:
  //    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  def compress[A](ls: List[A]): List[A] = {
    def compress_acc(prec: A, lst: List[A]): List[A] = lst match {
      case Nil => List(prec)
      case h :: Nil => if (h == prec) List(prec) else List(prec,h)
      case h :: tail => if (h == prec) compress_acc(prec, tail) else List(prec) ::: compress_acc(h, tail)
    }
    if (ls.isEmpty) List() else compress_acc(ls.head, ls.tail)
  }

  //		P09 (**) Pack consecutive duplicates of list elements into sublists.
  //		If a list contains repeated elements they should be placed in separate sublists.
  //		Example:
  //		scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //		res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[A](ls: List[A]): List[Any] = {
    def pack_acc(prec: A, run: List[A], lst: List[A]): List[Any] = lst match {
      case Nil => List(run)
      case h :: Nil => if (h == prec) List(run :+ h) else List(run,List(h))
      case h :: tail => if (h == prec) pack_acc(prec, run :+ h, tail) else List(run) ::: pack_acc(h, List(h), tail)
    }
    if (ls.isEmpty) List() else pack_acc(ls.head, List(ls.head), ls.tail)
  }


  //		P10 (*) Run-length encoding of a list.
  //		Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
  //		Example:
  //		scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //		res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[A](ls: List[A]): List[(Int, Any)] = {
    def encode_acc(prec: A, run: List[A], lst: List[A]): List[(Int, Any)] = lst match {
      case Nil => List((run.length, prec))
      case h :: Nil => if (h == prec) List((run.length + 1, h)) else List((run.length, prec),(1,h))
      case h :: tail => if (h == prec) encode_acc(prec, run :+ h, tail) else List((run.length, prec)) ::: encode_acc(h, List(h), tail)
    }
    if (ls.isEmpty) List() else encode_acc(ls.head, List(ls.head), ls.tail)
  }
  
  //		P11 (*) Modified run-length encoding.
  //		Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
  //		Example:
  //		scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //		res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

  //		P12 (**) Decode a run-length encoded list.
  //		Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  //		Example:
  //		scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  //		res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  //		P13 (**) Run-length encoding of a list (direct solution).
  //		Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  //		Example:
  //		scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //		res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  //		P14 (*) Duplicate the elements of a list.
  //		Example:
  //		scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  //		res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  //		P15 (**) Duplicate the elements of a list a given number of times.
  //		Example:
  //		scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  //		res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

  //		P16 (**) Drop every Nth element from a list.
  //		Example:
  //		scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //		res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  //		P17 (*) Split a list into two parts.
  //		The length of the first part is given. Use a Tuple for your result.
  //		Example:
  //		scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //		res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
}

