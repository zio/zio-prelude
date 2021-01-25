package zio.prelude

import zio.test._
import zio.test.laws._

object OrdJvmSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("OrdJvmSpec")(
      suite("laws")(
        testM("parSeq")(checkAllLaws(Ord)(Gen.listOf(Gen.anyInt).map(_.par)))
      )
    )
}
