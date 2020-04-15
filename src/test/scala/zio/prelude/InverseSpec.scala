package zio.prelude

import zio.prelude.newtypes.{ Prod, Sum }
import zio.test._
import zio.test.laws._
import zio.test.DefaultRunnableSpec

object InverseSpec extends DefaultRunnableSpec {
  def spec = suite("InverseSpec")(
    suite("laws")(
      testM("char addition")(checkAllLaws(Identity)(Gen.anyChar.map(Sum(_)))),
      testM("char multiplication")(checkAllLaws(Identity)(Gen.anyChar.map(Prod(_)))),
      testM("byte addition")(checkAllLaws(Identity)(Gen.anyByte.map(Sum(_)))),
      testM("byte multiplication")(checkAllLaws(Identity)(Gen.anyByte.map(Prod(_)))),
      testM("short addition")(checkAllLaws(Identity)(Gen.anyShort.map(Sum(_)))),
      testM("short multiplication")(checkAllLaws(Identity)(Gen.anyShort.map(Prod(_)))),
      testM("int addition")(checkAllLaws(Identity)(Gen.anyInt.map(Sum(_)))),
      testM("int multiplication")(checkAllLaws(Identity)(Gen.anyInt.map(Prod(_)))),
      testM("long addition")(checkAllLaws(Identity)(Gen.anyLong.map(Sum(_)))),
      testM("long multiplication")(checkAllLaws(Identity)(Gen.anyLong.map(Prod(_))))
    )
  )
}
