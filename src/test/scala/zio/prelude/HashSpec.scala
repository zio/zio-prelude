package zio.prelude

import zio.ZIO
import zio.test._
import zio.test.Assertion._
import zio.test.DefaultRunnableSpec

object HashSpec extends DefaultRunnableSpec {
  final def consistencyLaw[R, A: Hash : Equal] (gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    check(gen, gen) { (a1, a2) =>
      assert((a1 === a2) <==> (a1.hash === a2.hash))(isTrue ?? "consistencyLaw")
    }

  def spec = suite("HashSpec")(
    suite("laws")(
      testM("boolean")(consistencyLaw(Gen.boolean)),
      testM("byte")(consistencyLaw(Gen.anyByte)),
      testM("char")(consistencyLaw(Gen.anyChar)),
      testM("double")(consistencyLaw(Gen.anyDouble)),
      testM("either")(consistencyLaw(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("float")(consistencyLaw(Gen.anyFloat)),
      testM("int")(consistencyLaw(Gen.anyInt)),
      testM("list")(consistencyLaw(Gen.listOf(Gen.anyInt))),
      testM("long")(consistencyLaw(Gen.anyLong)),
      testM("map")(consistencyLaw(TestUtil.anyMap(Gen.anyInt, Gen.anyInt))),
      testM("option")(consistencyLaw(Gen.option(Gen.anyInt))),
      testM("set")(consistencyLaw(TestUtil.anySet(Gen.anyInt))),
      testM("string")(consistencyLaw(Gen.anyString)),
      testM("tuple2")(consistencyLaw(Gen.anyInt.zip(Gen.anyString))),
      testM("unit")(consistencyLaw(Gen.unit)),
      testM("vector")(consistencyLaw(Gen.vectorOf(Gen.anyInt)))
      )
    )
}
