package zio.prelude

import zio.prelude
import zio.prelude.newtypes._
import zio.prelude.Refinement.{And => _, Or => _, _}
import zio.test.Assertion._
import zio.test.AssertionM.Render.param
import zio.test._

object NewtypeSpec extends DefaultRunnableSpec {
  trait Dummy[A]

  type Age = Age.Type
  object Age extends Newtype[Int] {
    def refinement =
      refine(greaterThan(9) && lessThan(20))

    implicit val dummyType: Dummy[Age] = new Dummy[Age] {}
  }

  val cool = Age(12)
//  val cool: List[Age] = Age.wrapAll(12, 19, 15)

  val dummy = implicitly[Dummy[Age]]

  def spec =
    suite("NewtypeSpec")(
      suite("NewtypeSmart")(
        test("valid values") {
          assert(Natural(0))(equalTo(Natural.unsafeWrap(0)))
        },
        testM("invalid values") {
          val expected = "-1 did not satisfy greaterThanOrEqualTo(0)"
          assertM(typeCheck("Natural(-1)"))(isLeft(containsStringWithoutAnsi(expected)))
        }
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
          val z = x add y
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

  type Meter = Meter.Type
  object Meter extends Newtype[Double] {
    implicit final class MeterOps(private val self: Meter) extends AnyVal {
      def add(that: Meter): Meter =
        Meter.wrap(Meter.unwrap(self) + Meter.unwrap(that))
    }
  }

  def foldMap[A, B](as: List[A])(f: A => B)(implicit B: Identity[B]) =
    as.foldLeft(B.identity)((b, a) => B.combine(b, f(a)))

  def exists[A](as: List[A])(f: A => Boolean): Boolean =
    Or.unwrap(foldMap(as)(a => Or(f(a))))

  def forall[A](as: List[A])(f: A => Boolean): Boolean =
    And.unwrap(foldMap(as)(a => And(f(a))))

  object Natural extends Subtype[Int] {
    val refinement   = refine(prelude.Refinement.greaterThanOrEqualTo(0))
    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }
  type Natural = Natural.Type

  object IntSum extends Newtype[Int]
  type IntSum = IntSum.Type

  object IntMult extends Newtype[Int]
  type IntMult = IntMult.Type

  def sum[A](as: List[A])(implicit A: Identity[Sum[A]]): A =
    Sum.unwrap(Sum.wrapAll(as).foldLeft(A.identity)((b, a) => A.combine(b, a)))

  implicit class StringOps(private val self: String) extends AnyVal {
    def removingAnsiCodes: String =
      self.replaceAll("\u001B\\[[;\\d]*m", "")
  }

  def containsStringWithoutAnsi(element: String): Assertion[String] =
    Assertion.assertion("containsStringWithoutAnsi")(param(element))(_.removingAnsiCodes.contains(element))
}
