package zio.prelude

import zio.test._
import zio.test.laws._
import zio.test.DefaultRunnableSpec

object IdentitySpec extends DefaultRunnableSpec {
  def spec = suite("IdentitySpec")(
    suite("laws")(
      testM("char")(checkAllLaws(Equal)(Gen.anyChar)),
      testM("string")(checkAllLaws(Equal)(Gen.anyString)),
      testM("byte addition")(checkAllLaws(Equal)(Gen.anyByte.map(Sum(_)))),
      testM("byte multiplication")(checkAllLaws(Equal)(Gen.anyByte.map(Prod(_)))),
      testM("int addition")(checkAllLaws(Equal)(Gen.anyInt.map(Sum(_)))),
      testM("int multiplication")(checkAllLaws(Equal)(Gen.anyInt.map(Prod(_)))),
      testM("long addition")(checkAllLaws(Equal)(Gen.anyLong.map(Sum(_)))),
      testM("long multiplication")(checkAllLaws(Equal)(Gen.anyLong.map(Prod(_)))),
      testM("float")(checkAllLaws(Equal)(Gen.anyFloat.map(Sum(_)))),
      testM("double")(checkAllLaws(Equal)(Gen.anyDouble.map(Sum(_)))),
      testM("boolean disjunction")(checkAllLaws(Equal)(Gen.boolean.map(Disj(_)))),
      testM("boolean conjuction")(checkAllLaws(Equal)(Gen.boolean.map(Conj(_)))),
      testM("option")(checkAllLaws(Equal)(Gen.option(Gen.anyInt))),
      testM("either")(checkAllLaws(Equal)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("list")(checkAllLaws(Equal)(Gen.listOf(Gen.anyInt))),
      testM("vector")(checkAllLaws(Equal)(Gen.vectorOf(Gen.anyInt))),
      testM("map")(checkAllLaws(Equal)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
      testM("set")(checkAllLaws(Equal)(Gen.setOf(Gen.anyInt))),
      testM("tuple2")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyString))),
      testM("tuple3")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyString).zip(Gen.anyString)))
    )
  )
}
