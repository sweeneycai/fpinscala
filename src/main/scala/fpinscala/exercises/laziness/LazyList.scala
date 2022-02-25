package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() +: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => if n > 0 then Cons(h, () => t().take(n - 1)) else Empty

  def drop(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) => if n > 0 then t().drop(n - 1) else t() 

  def takeWhile(p: A => Boolean): LazyList[A] =
    this.foldRight(LazyList[A]())((a, b) => if p(a) then Cons(() => a, () => b) else b)

  def forAll(p: A => Boolean): Boolean = this match
     case Empty => true
     case Cons(x, xs) => if p(x()) then xs().forAll(p) else false

  def headOption: Option[A] = this match 
    case Empty => None
    case Cons(x, xs) =>
      try Some(x()) catch case e: Exception => None

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    this.foldRight(LazyList[B]())((a, b) => Cons(() => f(a), () => b))

  // def foldLeft[A]()
  
  def reverse: LazyList[A] = 
    this.foldRight(LazyList[A]())((a, b) => Cons(() => a, () => b))
  
  def append[B>:A](r: LazyList[B]): LazyList[B] =
    this.foldRight(r)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(LazyList[B]())((a, b) => f(a).append(b))

  def startsWith[B](s: LazyList[B]): Boolean = (this, s) match
    case (_, Empty) => true
    case (Cons(x1, xs1), Cons(x2, xs2)) => if x1() == x2() then xs1().startsWith(xs2()) else false 
    case _ => false


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
