package zio.prelude

import zio.test._
import zio.test.laws._

object DistributiveJoinMeetSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveJoinMeetSpec")(
      suite("laws")(
        testM("set")(checkAllLaws(DistributiveJoinMeet)(Gen.setOf(Gen.anyInt)))
      )
    )
}
