package zio.prelude

import zio.test._
import zio.test.Assertion._

object NewtypeSpec extends DefaultRunnableSpec {

  object Natural extends SubtypeSmart[Int](isGreaterThanEqualTo(0))
  type Natural = Natural.Type

  def isInvalid[E](assertion: Assertion[::[E]]): Assertion[Validation[E, Any]] =
    assertionRec("isInvalid")(Render.param(assertion))(assertion) {
      case Validation.Failure(e, es) => Some(::(e, es.toList))
      case _                         => None
    }

  def isValid[A](assertion: Assertion[A]): Assertion[Validation[Any, A]] =
    assertionRec("isValid")(Render.param(assertion))(assertion) {
      case Validation.Success(a) => Some(a)
      case _                     => None
    }

  def spec = suite("NewtypeSpec")(
    suite("NewtypeSmart")(
      test("valid values") {
        assert(Natural.make(0))(isValid(anything))
      },
      test("invalid values") {
        val expected = List("-1 did not satisfy isGreaterThanEqualTo(0)")
        assert(Natural.make(-1))(isInvalid(equalTo(expected)))
      }
    )
  )
}
