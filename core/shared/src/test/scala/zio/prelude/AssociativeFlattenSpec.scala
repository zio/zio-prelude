package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeFlattenSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeFlattenSpec")(
      suite("laws")(
        testM("map")(checkAllLaws(AssociativeFlatten)(GenFs.map(Gen.anyInt), Gen.anyInt))
      )
    )
}
