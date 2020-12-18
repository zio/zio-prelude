package zio.prelude

import zio.prelude.newtypes.{And, Or, Prod, Sum}
import zio.random.Random
import zio.test._
import zio.test.laws._

object CommutativeSpec extends DefaultRunnableSpec {

  val anySumInt: Gen[Random, Sum[Int]] = Gen.anyInt.map(Sum(_))

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeSpec")(
      suite("laws")(
        testM("char")(checkAllLaws(Commutative)(Gen.anyChar.map(Sum(_)))),
        testM("char")(checkAllLaws(Commutative)(Gen.anyChar.map(Prod(_)))),
        testM("byte addition")(checkAllLaws(Commutative)(Gen.anyByte.map(Sum(_)))),
        testM("byte multiplication")(checkAllLaws(Commutative)(Gen.anyByte.map(Prod(_)))),
        testM("short addition")(checkAllLaws(Commutative)(Gen.anyShort.map(Sum(_)))),
        testM("short multiplication")(checkAllLaws(Commutative)(Gen.anyShort.map(Prod(_)))),
        testM("int addition")(checkAllLaws(Commutative)(anySumInt)),
        testM("int multiplication")(checkAllLaws(Commutative)(Gen.anyInt.map(Prod(_)))),
        testM("long addition")(checkAllLaws(Commutative)(Gen.anyLong.map(Sum(_)))),
        testM("long multiplication")(checkAllLaws(Commutative)(Gen.anyLong.map(Prod(_)))),
        testM("boolean disjunction")(checkAllLaws(Commutative)(Gen.boolean.map(Or(_)))),
        testM("boolean conjuction")(checkAllLaws(Commutative)(Gen.boolean.map(And(_)))),
        testM("option")(checkAllLaws(Commutative)(Gen.option(anySumInt))),
        testM("either")(checkAllLaws(Commutative)(Gen.either(anySumInt, anySumInt))),
        testM("set")(checkAllLaws(Commutative)(Gen.setOf(anySumInt))),
        testM("map")(checkAllLaws(Commutative)(Gen.mapOf(anySumInt, anySumInt))),
        testM("tuple2")(checkAllLaws(Commutative)(anySumInt.zip(anySumInt))),
        testM("tuple3")(
          checkAllLaws(Commutative)(anySumInt.zip(anySumInt).zip(anySumInt).map { case ((x, y), z) => (x, y, z) })
        )
      )
    )
}
