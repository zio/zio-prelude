package zio.prelude

import com.github.ghik.silencer.silent
import zio.prelude.newtypes.{And, Or, Prod, Sum}
import zio.test.laws._
import zio.test.{testM, _}

object AssociativeSpec extends DefaultRunnableSpec {

  @silent("Unused import")
  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeSpec")(
      suite("laws")(
      )
    )
}
