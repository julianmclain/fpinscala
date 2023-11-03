package fpinscala.exercises.datastructures

import scala.annotation.tailrec

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

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
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

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else
      l match
        case Cons(_, t) => drop(t, n - 1)
        case Nil => Nil

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int = foldRight(l, 0, (_: A, acc: Int) => acc + 1)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, _*_)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    def loop(l: List[A], acc: List[A]): List[A] =
      l match
        case Nil => acc
        case Cons(h, t) => loop(t, Cons(h, acc))

    loop(l, List())

  def foldRightTailRec[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons.apply)

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int], (a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String], (a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight(l, Nil: List[B], (a: A, acc: List[B]) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight(as, Nil: List[A], (a: A, acc: List[A]) => if f(a) then Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = foldRight(as, List() : List[B], (a, b) => append(f(a), b))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, addPairwise(t, t2))
    case (Nil, _) => Nil
    case (_, Nil) => Nil

  def zipWith[A, B, C](x: List[A], y: List[B])(combine: (A, B) => C): List[C] = (x, y) match
    case (Cons(h, t), Cons(h2, t2)) => Cons(combine(h, h2), zipWith(t, t2)(combine))
    case (Nil, _) => Nil
    case (_, Nil) => Nil


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def startsWith(l: List[A], prefix: List[A]): Boolean = (l, prefix) match
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case (_, Nil) => true
      case _ => false

    sup match
      case Nil => sub == Nil
      case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)


  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean =
    def helper(sup: List[A], sub: List[A]): Boolean = (sup, sub) match
      case (Cons(h, t), Cons(h2, t2)) if h == h2 =>
        helper(t, t2)
      case (Nil, Nil) =>
        true
      case (l: Cons[A], Nil) =>
        true
      case (a, b) =>
        false

    def loop(l: List[A]): Boolean = l match
      case Nil => false
      case l @ Cons(_, t) => helper(l, sub) || loop(t)

    loop(sup)

object Main extends App:
  import fpinscala.exercises.datastructures

  val l = datastructures.List((1 to 1_000_000).toList*)
  println(datastructures.List.length(l))

