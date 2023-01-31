package zio.prelude

import zio.test.Assertion._
import zio.test.{Spec, ZIOSpecDefault, assert}

object AssertionSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("Assertion")(
    test("matches must fail when the regex only match a part of the string") {
      assert((Assertion.matches("biking").apply("toto like biking")))(isLeft(anything))
    },
    test("matches must fail when a regexp using | only match a part of the string") {
      assert((Assertion.matches("swimming|biking").apply("toto like biking")))(isLeft(anything))
    },
    test("matches must work when the regex only match the full string") {
      assert((Assertion.matches("t.*g").apply("toto like biking")))(isRight(isUnit))
    },
    suite("fromFunction")(
      test("must succeed if function returns true") {
        assert(isBlank.apply(""))(isRight(isUnit))
      },
      test("must fail if function returns false") {
        assert(isBlank.apply("non-blank"))(isLeft(anything))
      }
    )
  )

  private def isBlank: Assertion[String] =
    Assertion.fromFunction[String]("isBlank")(_.isBlank)
}
