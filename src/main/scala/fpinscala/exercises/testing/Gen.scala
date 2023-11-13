package fpinscala.exercises.testing

import fpinscala.answers.state.*
import fpinscala.answers.parallelism.*
import Gen.*
import Prop.*
import Prop.Result.*
import fpinscala.answers.state.RNG.Simple

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true
      case Proved => false

  extension (self: Prop)
    def check(maxSize: MaxSize = 100, testCases: TestCases  = 100, rng: RNG = RNG.Simple(System.currentTimeMillis())): Result =
      self(maxSize, testCases, rng)

    def run(maxSize: MaxSize = 100,
            testCases: TestCases = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")

    def &&(that: Prop): Prop =
      (maxSize, testCases, rng) =>
        val r1 = self.check(maxSize, testCases, rng)
        if !r1.isFalsified then that.check(maxSize, testCases, rng) else r1

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop:
    (n, rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) =>
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(a, e), i)
    }.find(_.isFalsified).getOrElse(Passed)

  @annotation.targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList.from(0)
          .take((n.toInt min max.toInt) + 1)
          .map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map[Prop](p => (max, n, rng) =>
          p(max, casesPerSize, rng))
          .toList
          .reduce(_ && _)
      prop(max, n, rng)

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)


opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension[A] (self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)

    def map[B](f: A => B): Gen[B] = State.map(self)(f)

    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def unsized: SGen[A] = _ => self

    def list: SGen[List[A]] = n => self.listOfN(n)

    def nonEmptyList: SGen[List[A]] = n => self.listOfN(n.max(1))

  def unit[A](a: => A): Gen[A] = State.unit(a)

  def boolean: Gen[Boolean] = State(RNG.boolean)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(i => (i + start) % stopExclusive)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)


opaque type SGen[+A] = Int => Gen[A]

object SGen:
  extension[A] (self: SGen[A])
    def flatMap[B](f: A => SGen[B]): SGen[B] = n => self(n).flatMap(a => f(a)(n))

    def map[B](f: A => B): SGen[B] = n => self(n).map(f)


object Main extends App:
  val rng = Simple(1L)
  val loi = Gen.choose(1, 100).listOfN(10).next(rng)
  println(loi)

  val smallInt = Gen.choose(-10, 10)

  // all elements are less than or equal to the max
  val maxProp1 = Prop.forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.forall(_ <= max)
  maxProp1.run()

  // the max is an element of the list
  val maxProp2 = Prop.forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.contains(max)
  maxProp2.run()

// each element is greater than or equal to the previous
  val sortedProp = Prop.forAll(smallInt.list): l =>
    val sortedList = l.sorted
    def isSorted(l: List[Int]): Boolean = l match
      case Nil => true
      case x :: Nil => true
      case x :: y :: xs => if x <= y then isSorted(xs) else false

    isSorted(sortedList)

  sortedProp.run()



