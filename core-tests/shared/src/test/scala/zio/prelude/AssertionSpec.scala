package zio.prelude

import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, ZSpec, assert}

object AssertionSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Nothing] = suite("Assertion")(
    test("matches must fail when the regex only match a part of the string") {
      assert((Assertion.matches("biking").apply("toto like biking")))(isLeft(anything))
    },
    test("matches must fail when a regexp using | only match a part of the string") {
      assert((Assertion.matches("swimming|biking").apply("toto like biking")))(isLeft(anything))
    },
    test("matches must work when the regex only match the full string") {
      assert((Assertion.matches("t.*g").apply("toto like biking")))(isRight(isUnit))
    }
  )
}
