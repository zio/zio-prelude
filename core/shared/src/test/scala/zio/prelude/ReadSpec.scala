package zio.prelude

import zio.test._
import zio.test.laws.checkAllLaws

object ReadSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("ReadSpec")(
      suite("laws")(
        testM("boolean")(checkAllLaws(Read)(Gen.boolean)),
        testM("byte")(checkAllLaws(Read)(Gen.anyByte)),
        testM("char")(checkAllLaws(Read)(Gen.anyChar)),
        testM("double")(checkAllLaws(Read)(Gen.anyDouble)),
        testM("float")(checkAllLaws(Read)(Gen.anyFloat)),
        testM("int")(checkAllLaws(Read)(Gen.anyInt)),
        testM("long")(checkAllLaws(Read)(Gen.anyLong)),
        testM("short")(checkAllLaws(Read)(Gen.anyShort)),
        testM("string")(checkAllLaws(Read)(Gen.anyString))
      )
    )
}
