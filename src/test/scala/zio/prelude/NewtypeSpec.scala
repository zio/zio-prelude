package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.coherent.AssociativeIdentity
import zio.prelude.newtypes._
import zio.test._
import zio.test.Assertion._

object NewtypeSpec extends DefaultRunnableSpec {

  def spec = suite("NewtypeSpec")(
    suite("NewtypeSmart")(
      test("valid values") {
        assert(Natural.make(0))(isSuccessV(anything))
      },
      test("invalid values") {
        val expected = NonEmptyChunk("-1 did not satisfy isGreaterThanEqualTo(0)")
        assert(Natural.make(-1))(isFailureV(equalTo(expected)))
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

  def foldMap[A, B](as: List[A])(f: A => B)(implicit B: AssociativeIdentity[B]) =
    as.foldLeft(B.identity)((b, a) => B.combine(b, f(a)))

  def exists[A](as: List[A])(f: A => Boolean): Boolean =
    Or.unwrap(foldMap(as)(a => Or(f(a))))

  def forall[A](as: List[A])(f: A => Boolean): Boolean =
    And.unwrap(foldMap(as)(a => And(f(a))))

  object Natural extends SubtypeSmart[Int](isGreaterThanEqualTo(0))
  type Natural = Natural.Type

  object IntSum extends Newtype[Int]
  type IntSum = IntSum.Type

  object IntMult extends Newtype[Int]
  type IntMult = IntMult.Type

  def sum[A](as: List[A])(implicit A: AssociativeIdentity[Sum[A]]): A =
    Sum.unwrap(Sum.wrapAll(as).foldLeft(A.identity)((b, a) => A.combine(b, a)))
}
