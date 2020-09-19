package zio.prelude

import zio.test._
import zio.test.laws._

object ContravariantSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ContravariantSpec")(
      suite("laws")(
        // testM("equal")(checkAllLaws(Contravariant)(???, Gen.anyInt))
      )
    )
}