package zio.prelude

import zio.prelude.newtypes.{ And, Or, Prod, Sum }
import zio.test.{ testM, _ }
import zio.test.laws._

object ClosureSpec extends DefaultRunnableSpec {

  def spec = suite("ClosureSpec")(
    suite("laws")(
      testM("char")(checkAllLaws(Closure)(Gen.anyChar.map(Sum(_)))),
      testM("char")(checkAllLaws(Closure)(Gen.anyChar.map(Prod(_)))),
      testM("string")(checkAllLaws(Closure)(Gen.anyString)),
      testM("byte addition")(checkAllLaws(Closure)(Gen.anyByte.map(Sum(_)))),
      testM("byte multiplication")(checkAllLaws(Closure)(Gen.anyByte.map(Prod(_)))),
      testM("short addition")(checkAllLaws(Closure)(Gen.anyShort.map(Sum(_)))),
      testM("short multiplication")(checkAllLaws(Closure)(Gen.anyShort.map(Prod(_)))),
      testM("int addition")(checkAllLaws(Closure)(Gen.anyInt.map(Sum(_)))),
      testM("int multiplication")(checkAllLaws(Closure)(Gen.anyInt.map(Prod(_)))),
      testM("long addition")(checkAllLaws(Closure)(Gen.anyLong.map(Sum(_)))),
      testM("long multiplication")(checkAllLaws(Closure)(Gen.anyLong.map(Prod(_)))),
      testM("boolean disjunction")(checkAllLaws(Closure)(Gen.boolean.map(Or(_)))),
      testM("boolean conjuction")(checkAllLaws(Closure)(Gen.boolean.map(And(_)))),
      testM("option")(checkAllLaws(Closure)(Gen.option(Gen.anyString))),
      testM("list")(checkAllLaws(Closure)(Gen.listOf(Gen.anyString))),
      testM("vector")(checkAllLaws(Closure)(Gen.vectorOf(Gen.anyString))),
      testM("map")(checkAllLaws(Closure)(Gen.mapOf(Gen.anyString, Gen.anyString))),
      testM("set")(checkAllLaws(Closure)(Gen.setOf(Gen.anyString))),
      testM("tuple2")(checkAllLaws(Closure)(Gen.anyString.zip(Gen.anyString))),
      testM("tuple3")(checkAllLaws(Closure)(Gen.anyString.zip(Gen.anyString).zip(Gen.anyString))),
      testM("chunk")(checkAllLaws(Closure)(Gens.chunkOf(Gen.anyString)))
    )
  )
}
