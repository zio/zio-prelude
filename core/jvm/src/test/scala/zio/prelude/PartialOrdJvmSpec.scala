package zio.prelude

import zio.test._
import zio.test.laws._

object PartialOrdJvmSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("EqualJvmSpec")(
      suite("laws")(
        testM("parMap")(checkAllLaws(PartialOrd)(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        testM("parSet")(checkAllLaws(PartialOrd)(Gen.setOf(Gen.anyInt).map(_.par)))
      )
    )
}
