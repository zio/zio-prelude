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
        testM("char addition")(checkAllLaws(Associative)(Gen.anyChar.map(Sum(_)))),
        testM("char multiplication")(checkAllLaws(Associative)(Gen.anyChar.map(Prod(_)))),
        testM("string")(checkAllLaws(Associative)(Gen.anyString)),
        testM("byte addition")(checkAllLaws(Associative)(Gen.anyByte.map(Sum(_)))),
        testM("byte multiplication")(checkAllLaws(Associative)(Gen.anyByte.map(Prod(_)))),
        testM("short addition")(checkAllLaws(Associative)(Gen.anyShort.map(Sum(_)))),
        testM("short multiplication")(checkAllLaws(Associative)(Gen.anyShort.map(Prod(_)))),
        testM("int addition")(checkAllLaws(Associative)(Gen.anyInt.map(Sum(_)))),
        testM("int multiplication")(checkAllLaws(Associative)(Gen.anyInt.map(Prod(_)))),
        testM("long addition")(checkAllLaws(Associative)(Gen.anyLong.map(Sum(_)))),
        testM("long multiplication")(checkAllLaws(Associative)(Gen.anyLong.map(Prod(_)))),
        testM("boolean disjunction")(checkAllLaws(Associative)(Gen.boolean.map(Or(_)))),
        testM("boolean conjuction")(checkAllLaws(Associative)(Gen.boolean.map(And(_)))),
        testM("option")(checkAllLaws(Associative)(Gen.option(Gen.anyInt.map(Sum(_))))),
        testM("list")(checkAllLaws(Associative)(Gen.listOf(Gen.anyInt))),
        testM("vector")(checkAllLaws(Associative)(Gen.vectorOf(Gen.anyInt))),
        testM("map")(checkAllLaws(Associative)(Gen.mapOf(Gen.anyInt, Gen.anyInt.map(Sum(_))))),
        testM("set")(checkAllLaws(Associative)(Gen.setOf(Gen.anyInt))),
        testM("tuple2")(checkAllLaws(Associative)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))))),
        testM("tuple3")(
          checkAllLaws(Associative)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))).zip(Gen.anyInt.map(Sum(_))))
        ),
        testM("chunk")(checkAllLaws(Associative)(Gen.chunkOf(Gen.anyInt)))
      )
    )
}
