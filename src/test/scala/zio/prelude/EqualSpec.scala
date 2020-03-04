package zio.prelude

import zio.test._
import zio.test.Assertion._

object EqualSpec extends DefaultRunnableSpec {

  def spec = suite("EqualSpec")(
    suite("instances")(
      suite("unit")(
        test("equal is consistent with universal equality") {
          assert(Equal[Unit].equal((), ()))(isTrue)
        }
      )
    )
  )
}
