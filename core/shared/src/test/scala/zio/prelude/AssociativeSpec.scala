package zio.prelude

import com.github.ghik.silencer.silent
import zio.test._

object AssociativeSpec extends DefaultRunnableSpec {

  @silent("Unused import")
  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeSpec")(
      suite("laws")(
      )
    )
}
