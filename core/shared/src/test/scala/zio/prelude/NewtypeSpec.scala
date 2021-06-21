package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.newtypes._
import zio.prelude.macros.Refined
import zio.test.Assertion._
import zio.test._

object NewtypeSpec extends DefaultRunnableSpec {
  trait Dummy[A]

  object Age extends Newtype[Int] {
    // type Instances
    implicit val dummyType: Dummy[Age] = new Dummy[Age] {}
  }
  // Age.newtype.Type with Age.Instances
  type Age = Age.Type

  val dummy                                     = implicitly[Dummy[Age]]
  def removingAnsiCodes(string: String): String = string.replaceAll("\u001B\\[[;\\d]*m", "");

  def spec =
    suite("NewtypeSpec")(
      suite("NewtypeSmart")(
        test("valid values") {
          assert(Natural.make(0))(isSuccessV(anything))
        },
        test("invalid values") {
          val expected = "• -1 did not satisfy greaterThan(-1)"
          assert(Natural.make(-1).mapError(removingAnsiCodes))(isFailureV(equalTo(NonEmptyChunk(expected))))
        },
        test("valid values at compile time") {
          assert(Natural(0))(anything)
        },
        testM("invalid values at compile time") {
          assertM(typeCheck("Natural(-1)"))(
            isLeft(containsString("greaterThan(-1)") && containsString("Assertion Failed"))
          )
        },
      ),
      suite("SubtypeSmart")(
        test("subtypes values") {
          val two = 2
          assert(two + Natural.two)(equalTo(2 + 2))
        }
      ),
      suite("examples from documentation")(
        test("meter") {
          val x = Meter(3.4)
          val y = Meter(4.3)
          val z = x + y
          assert(Meter.unwrap(z))(equalTo(3.4 + 4.3))
        },
        test("exists") {
          assert(exists(List(true, false))(identity))(isTrue)
        },
        test("forall") {
          assert(forall(List(true, false))(identity))(isFalse)
        },
        test("sumInt") {
          val actual   = sum(List(1, 2, 3))
          val expected = 6
          assert(actual)(equalTo(expected))
        },
        test("sumLong") {
          val actual   = sum(List(1L, 2L, 3L))
          val expected = 6L
          assert(actual)(equalTo(expected))
        }
      )
    )

  object Meter extends Newtype[Double]
  type Meter = Meter.Type

  implicit class MeterSyntax(private val self: Meter) extends AnyVal {
    def +(that: Meter): Meter =
      Meter.wrap(Meter.unwrap(self) + Meter.unwrap(that))
  }

  def foldMap[A, B](as: List[A])(f: A => B)(implicit B: Identity[B]) =
    as.foldLeft(B.identity)((b, a) => B.combine(b, f(a)))

  def exists[A](as: List[A])(f: A => Boolean): Boolean =
    Or.unwrap(foldMap(as)(a => Or(f(a))))

  def forall[A](as: List[A])(f: A => Boolean): Boolean =
    And.unwrap(foldMap(as)(a => And(f(a))))

  val NaturalType = Refined[Int](zio.prelude.refined.Assertion.greaterThan(-1) )
  object Natural extends NaturalType.Subtype {
    val two: Natural = Natural(10)
  }
  type Natural = Natural.Type

  val newValue = Natural(10) + 3
  val hash = implicitly[Hash[Natural]]
  hash.hash(Natural(8))

  object IntSum extends Newtype[Int]
  type IntSum = IntSum.Type

  object IntMult extends Newtype[Int]
  type IntMult = IntMult.Type

  def sum[A](as: List[A])(implicit A: Identity[Sum[A]]): A =
    Sum.unwrap(Sum.wrapAll(as).foldLeft(A.identity)((b, a) => A.combine(b, a)))
}


object Cool {
  import NewtypeSpec.Natural
  val hash = implicitly[Hash[Natural]]
  Natural(10)
}

