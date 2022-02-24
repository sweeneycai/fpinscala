package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match 
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match 
    case Left(e) => b
    case Right(a) => Right(a)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
    case (Right(a), Right(b)) => Right(f(a, b))

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    es.foldRight[Either[E, List[B]]](Right(List[B]())) { (a, b) => 
      f(a).map2(b)((a, b) => a +: b)
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = (a, b) match 
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)
    case (Left(e1), _) => Left(e1)
    case (_, Left(e2)) => Left(e2)
    case (Right(a), Right(b)) => Right(f(a, b))

  def traverseAll[E, A, B](es: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
