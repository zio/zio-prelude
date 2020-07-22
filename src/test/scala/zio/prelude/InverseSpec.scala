package zio.prelude

import zio.prelude.newtypes.{ Sum }
import zio.test._
import zio.test.laws._
import zio.test.DefaultRunnableSpec

object InverseSpec extends DefaultRunnableSpec {
  def spec = suite("InverseSpec")(
    suite("laws")(
      testM("char addition")(checkAllLaws(Inverse)(Gen.anyChar.map(Sum(_)))),
      testM("byte addition")(checkAllLaws(Inverse)(Gen.anyByte.map(Sum(_)))),
      testM("short addition")(checkAllLaws(Inverse)(Gen.anyShort.map(Sum(_)))),
      testM("int addition")(checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)))),
      testM("long addition")(checkAllLaws(Inverse)(Gen.anyLong.map(Sum(_)))),
      testM("set")(checkAllLaws(Inverse)(Gen.setOf(Gen.anyInt)))
    )
  )
}
