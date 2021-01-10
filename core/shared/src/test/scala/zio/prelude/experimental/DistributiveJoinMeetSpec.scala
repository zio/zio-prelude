package zio.prelude
package experimental

import zio.test._
import zio.test.laws._

object DistributiveJoinMeetSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveJoinMeetSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(DistributiveJoinMeet)(Gen.setOf(Gen.anyInt))),
        testM("boolean")(checkAllLaws(DistributiveJoinMeet)(Gen.anyInt.map(_ % 2 == 0)))
      )
    )
}
