package zio.prelude

import zio.prelude.laws.IdentityLaws
import zio.prelude.newtypes._
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object IdentitySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("IdentitySpec")(
      suite("laws")(
        testM("boolean conjuction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(And(_)))),
        testM("boolean disjunction")(checkAllLaws(IdentityLaws)(Gen.boolean.map(Or(_)))),
        testM("byte multiplication")(checkAllLaws(IdentityLaws)(Gen.anyByte.map(Prod(_)))),
        testM("char multiplication")(checkAllLaws(IdentityLaws)(Gen.anyChar.map(Prod(_)))),
        testM("chunk")(checkAllLaws(IdentityLaws)(Gen.chunkOf(Gen.anyInt))),
        testM("double multiplication")(checkAllLaws(IdentityLaws)(Gen.anyDouble.map(Prod(_)))),
        testM("duration ZIO")(checkAllLaws(IdentityLaws)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(IdentityLaws)(Gen.either(Gen.anyInt.map(Sum(_)), Gen.anyInt.map(Sum(_))))),
        testM("float multiplication")(checkAllLaws(IdentityLaws)(Gen.anyFloat.map(Prod(_)))),
        testM("int multiplication")(checkAllLaws(IdentityLaws)(Gen.anyInt.map(Prod(_)))),
        testM("list")(checkAllLaws(IdentityLaws)(Gen.listOf(Gen.anyInt))),
        testM("long multiplication")(checkAllLaws(IdentityLaws)(Gen.anyLong.map(Prod(_)))),
        testM("map")(checkAllLaws(IdentityLaws)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        testM("option")(checkAllLaws(IdentityLaws)(Gen.option(Gen.anyInt.map(Sum(_))))),
        testM("short multiplication")(checkAllLaws(IdentityLaws)(Gen.anyShort.map(Prod(_)))),
        testM("string")(checkAllLaws(IdentityLaws)(Gen.anyString)),
        testM("vector")(checkAllLaws(IdentityLaws)(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
