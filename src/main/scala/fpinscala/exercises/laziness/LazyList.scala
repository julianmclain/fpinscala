package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.Empty

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListTailRec: List[A] =
    @annotation.tailrec
    def loop(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)

    loop(this, List.empty[A]).reverse

  def toList: List[A] =
    val buff = scala.collection.mutable.ListBuffer.empty[A]
    @tailrec
    def go(ll: LazyList[A]): List[A] = ll match
      case Empty => buff.toList
      case Cons(h, t) =>
        buff += h()
        go(t())

    go(this)

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

  def take(n: Int): LazyList[A] =
    LazyList.unfold((this, n)):
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None

  def take2(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => LazyList.cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => LazyList.cons(h(), LazyList.empty)
    case _ =>  LazyList.empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None

  def takeWhile2(p: A => Boolean): LazyList[A] =
    this.foldRight(LazyList.empty)((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this.foldRight(Option.empty)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map3[B](f: A => B): LazyList[B] = this match
    case Cons(h, t) => LazyList.cons(f(h()), t().map3(f))
    case Empty => LazyList.empty

  def map2[B](f: A => B): LazyList[B] =
    this.flatMap(a => LazyList.apply(f(a)))

  def map4[B](f: A => B): LazyList[B] =
    this.foldRight(LazyList.empty)((a, b) => LazyList.cons(f(a), b))

  def map[B](f: A => B): LazyList[B] =
    LazyList.unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    this.foldRight(that)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(LazyList.empty)((a, b) => f(a).append(b))

  def flatMap2[B](f: A => LazyList[B]): LazyList[B] = this match
    case Cons(h, t) => f(h()).append(t().flatMap2(f))
    case Empty => LazyList.empty

  def filter(p: A => Boolean): LazyList[A] =
    this.foldRight(LazyList.empty)((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def startsWith[B](s: LazyList[B]): Boolean =
    this.zipAll(s).map:
      case (Some(a), Some(b)) => a == b
      case (_, None) => true
      case (None, Some(_)) => false
    .forAll(_ == true)

  def startsWith2[B](s: LazyList[B]): Boolean = (this, s) match
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => t1().startsWith(t2())
    case (Empty, Empty) => true
    case (_, Empty) => true
    case (_, _) => false

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case _ => None

  def hasSubsequence[B](that: LazyList[B]): Boolean = this match
    case l @ Cons(_, t) => l.startsWith(that) || t().hasSubsequence(that)
    case Empty => that == Empty

  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(this):
      case l @ Cons(_, t) => Some((l, t()))
      case Empty => None
    .append(LazyList(LazyList.empty))

  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] =
    this.tails.map(l => l.foldRight(z)(f))

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

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def loop(curr: Int, next: Int): LazyList[Int] =
      LazyList.cons(curr, loop(next, curr + next))

    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => LazyList.cons(a, unfold(s)(f))
      case None => LazyList.empty

  lazy val fibsViaUnfold: LazyList[Int] =
    LazyList.unfold((0, 1)){ case (curr, next) => Some(curr, (next, curr + next)) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    LazyList.unfold(n)(num => Some((num, num+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    LazyList.unfold(a)(s => Some(s, s))

  lazy val onesViaUnfold: LazyList[Int] =
    LazyList.unfold(1)(_ => Some(1, 1))
