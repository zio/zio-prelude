package zio.prelude

import zio.test._
import zio.test.laws._

object ComplementSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ComplementSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(Complement)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
