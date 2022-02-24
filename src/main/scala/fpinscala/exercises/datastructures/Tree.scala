package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match 
    case Leaf(_) => 1
    case Branch(l, r) => math.max(l.depth + 1, r.depth + 1)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match 
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  
  def sizeViaFold: Int =
    this.fold(_ => 1, _ + _)
  
  def depthViaFold: Int = 
    this.fold(_ => 1, (a, b) => math.max(a +1 , b + 1))
  
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), (a, b) => Branch(a, b))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match 
    case Leaf(a) => if a > 0 then Some(a) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive 

  extension (t: Tree[Int]) def maximum: Int = 
    t.fold(a => a, math.max)

  extension (t: Tree[Int]) def maximumViaFold: Int = 
    t.fold(a => a, math.max)
