package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (a, rng2) = rng.nextInt
    (if a < 0 then -(a - 1) else a, rng2)

  def _double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def double(rng: RNG): (Double, RNG) =
    val (i, rng2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    map2(int, double)((_, _))(rng)

  def intDouble2(rng: RNG): ((Int,Double), RNG) =
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)


  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    map2(double, int)((_, _))(rng)

  def doubleInt2(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    intsRand(count)(rng)

  def intsRand(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(rng => rng.nextInt))

  def intsRec(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then
      (Nil, rng)
    else
      val (x, r) = rng.nextInt
      val (xs, r2) = ints(count-1)(r)
      (x :: xs, r2)

  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (acc, rng)
      else
        val (i, r) = rng.nextInt
        loop(count-1, r, i :: acc)

    loop(count, rng, Nil)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      (f(a, b), r3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft[Rand[List[A]]](unit(Nil)):
      (rb, ra) =>
        rng =>
          val (a, r2) = ra(rng)
          val (b, r3) = rb(r2)
          (a :: b, r3)

  def sequence2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft[Rand[List[A]]](unit(Nil))((b, a) => map2(a, b)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r2) = r(rng)
      f(a)(r2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))


opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = run(s)
        f(a)(s2)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    traverse(actions)(a => a)

  def traverse[S, A, B](actions: List[State[S, A]])(fn: A => B): State[S, List[B]] =
    actions.foldRight(unit(List.empty[B]))((f, b) => f.map2(b)((aa, bb) => fn(aa) :: bb))

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???


/*
Notes
- The Rand[A] type alias is really hard to wrap your head around
- Random observation: with these super abstract functions it's really hard to implement
  them incorrectly. You're pretty constrained by the types. You either can't figure out
  how to implement it at all or it's correct.
- It would be nice to walk through an example of sequence
- What is the intuition behind what flatmap does? For example, map is easy: within
  the context of an F, I want to transform an A to B.
  - Is this state pass along something that's intrinsic to flatmap or is it a detail of
  Rand / State? Seems like other Monads don't have this behavior: Option, Either, Future, etc...
- opaque type is confusing re-review before meeting

*/