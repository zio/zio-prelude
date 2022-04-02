package zio.prelude

import zio.test._

object AssociativeSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeSpec")(
      suite("laws")(
      )
    )
}
