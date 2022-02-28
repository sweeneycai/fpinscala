package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, _ + _)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def empty[A] = Nil

  def tail[A](l: List[A]): List[A] = l match
    case Nil => Nil
    case Cons(x, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] = l match 
    case Nil => Cons(h,  Nil)
    case Cons(x, xs) => Cons(h, xs)

  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(x, xs) => 
      if n > 0 then drop(xs, n - 1) else Cons(x, drop(xs, n - 1))
    case _ => Nil

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match 
    case Nil => Nil
    case Cons(x, xs) => if f(x) then dropWhile(xs, f) else Cons(x, dropWhile(xs, f))

  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("Empty List")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (a, b) => b + 1)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0, (b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (b, a) => Cons(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(r, l, (a, b) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, List[A](), appendViaFoldRight)

  def incrementEach(l: List[Int]): List[Int] =
    map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] =
    map(l)(_.toString)

  def map[A,B](l: List[A])(f: A => B): List[B] = l match 
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A](), (a, b) => if f(a) then Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, List[B](), (a, b) => appendViaFoldRight(f(a), b))

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b)(_ + _)

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))

  // def zipWith - TODO determine signature

  def startWith[A](prefix: List[A], list: List[A]): Boolean = (prefix, list) match 
    case (Nil, _) => true
    case (_, Nil) => false // prefix is longer than list 
    case (Cons(x1, xs1), Cons(x2, xs2)) =>
      if x1 == x2 then startWith(xs1, xs2) else false
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match 
    case Nil => false
    case Cons(x, xs) => if startWith(sub, sup) then true else hasSubsequence(xs, sub)
