package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.Refinement.{And => _, Or => _}
import zio.prelude.newtypes._
import zio.test.Assertion._
import zio.test.AssertionM.Render.param
import zio.test._

object NewtypeSpec extends DefaultRunnableSpec {
  trait Dummy[A]

  type Age = Age.Type
  object Age extends Newtype[Int] {
    implicit val dummyType: Dummy[Age] = new Dummy[Age] {}
  }

  val dummy = implicitly[Dummy[Age]]

  def spec =
    suite("NewtypeSpec")(
      suite("NewtypeSmart")(
        test("valid values at compile-time") {
          assertTrue(Natural(0) == Natural.unsafeWrap(0))
        },
        test("multiple valid values at compile-time") {
          assertTrue(
            Natural(0, 1, 900) == NonEmptyChunk(Natural.unsafeWrap(0), Natural.unsafeWrap(1), Natural.unsafeWrap(900))
          )
        },
        test("valid values at run-time") {
          assert(Natural.make(0))(isSuccessV(equalTo(Natural.unsafeWrap(0))))
        },
        testM("invalid values at compile-time") {
          assertM(typeCheck("Natural(-1, -8, 4, -3)"))(
            isLeft(
              containsStringWithoutAnsi("-1 did not satisfy greaterThanOrEqualTo(0)") &&
                containsStringWithoutAnsi("-8 did not satisfy greaterThanOrEqualTo(0)") &&
                containsStringWithoutAnsi("-3 did not satisfy greaterThanOrEqualTo(0)")
            )
          )
        } @@ TestAspect.ignore,
        testM("invalid value at run-time") {
          assertM(typeCheck("Natural(-1)"))(
            isLeft(containsStringWithoutAnsi("-1 did not satisfy greaterThanOrEqualTo(0)"))
          )
        } @@ TestAspect.ignore,
        test("invalid values at run-time") {
          assert(Natural.make(-1))(
            isFailureV(equalTo(NonEmptyChunk("-1 did not satisfy greaterThanOrEqualTo(0)")))
          )
        }
      ),
      suite("SubtypeSmart")(
        test("subtypes values") {
          val two = 2
          assertTrue(two + Natural.two == 2 + 2)
        }
      ),
      suite("examples from documentation")(
        test("meter") {
          val x = Meter(3.4)
          val y = Meter(4.3)
          val z = x add y
          assertTrue(Meter.unwrap(z) == 3.4 + 4.3)
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
          assertTrue(actual == expected)
        },
        test("sumLong") {
          val actual   = sum(List(1L, 2L, 3L))
          val expected = 6L
          assertTrue(actual == expected)
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
    Or.unwrap(foldMap(as)(a => Or.create(f(a))))

  def forall[A](as: List[A])(f: A => Boolean): Boolean =
    And.unwrap(foldMap(as)(a => And.create(f(a))))

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override inline def refinement = Refinement.greaterThanOrEqualTo(0)

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }

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
