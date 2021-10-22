package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.test._
import zio.test.laws._

object DistributiveJoinMeetSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveJoinMeetSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(DistributiveJoinMeetLaws)(Gen.setOf(Gen.anyInt))),
        testM("boolean")(checkAllLaws(DistributiveJoinMeetLaws)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
