package zio.prelude

import zio.test._

object AssociativeSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeSpec")(
      suite("laws")(
      )
    )
}
