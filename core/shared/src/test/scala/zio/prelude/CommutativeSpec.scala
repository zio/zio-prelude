package zio.prelude

import zio.prelude.Common.anyFiniteDurationScala
import zio.prelude.newtypes.{And, Max, Min, Or, Prod, Sum}
import zio.random.Random
import zio.test._
import zio.test.laws._

object CommutativeSpec extends DefaultRunnableSpec {

  val anySumInt: Gen[Random, Sum[Int]] = Gen.anyInt.map(Sum(_))

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeSpec")(
      suite("laws")(
        testM("boolean conjuction")(checkAllLaws(Commutative)(Gen.boolean.map(And(_)))),
        testM("boolean disjunction")(checkAllLaws(Commutative)(Gen.boolean.map(Or(_)))),
        testM("byte addition")(checkAllLaws(Commutative)(Gen.anyByte.map(Sum(_)))),
        testM("byte multiplication")(checkAllLaws(Commutative)(Gen.anyByte.map(Prod(_)))),
        testM("char addition")(checkAllLaws(Commutative)(Gen.anyChar.map(Sum(_)))),
        testM("char multiplication")(checkAllLaws(Commutative)(Gen.anyChar.map(Prod(_)))),
        testM("double addition")(checkAllLaws(Commutative)(Gen.anyDouble.map(Sum(_)))),
        testM("double max")(checkAllLaws(Commutative)(Gen.anyDouble.map(Max(_)))),
        testM("double min")(checkAllLaws(Commutative)(Gen.anyDouble.map(Min(_)))),
        testM("double multiplication")(checkAllLaws(Commutative)(Gen.anyDouble.map(Prod(_)))),
        testM("duration Scala")(checkAllLaws(Commutative)(anyFiniteDurationScala)),
        testM("duration ZIO")(checkAllLaws(Commutative)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(Commutative)(Gen.either(anySumInt, anySumInt))),
        testM("float addition")(checkAllLaws(Commutative)(Gen.anyFloat.map(Sum(_)))),
        testM("float max")(checkAllLaws(Commutative)(Gen.anyFloat.map(Max(_)))),
        testM("float min")(checkAllLaws(Commutative)(Gen.anyFloat.map(Min(_)))),
        testM("float multiplication")(checkAllLaws(Commutative)(Gen.anyFloat.map(Prod(_)))),
        testM("int addition")(checkAllLaws(Commutative)(anySumInt)),
        testM("int multiplication")(checkAllLaws(Commutative)(Gen.anyInt.map(Prod(_)))),
        testM("long addition")(checkAllLaws(Commutative)(Gen.anyLong.map(Sum(_)))),
        testM("long multiplication")(checkAllLaws(Commutative)(Gen.anyLong.map(Prod(_)))),
        testM("map")(checkAllLaws(Commutative)(Gen.mapOf(anySumInt, anySumInt))),
        testM("option")(checkAllLaws(Commutative)(Gen.option(anySumInt))),
        testM("set")(checkAllLaws(Commutative)(Gen.setOf(anySumInt))),
        testM("short addition")(checkAllLaws(Commutative)(Gen.anyShort.map(Sum(_)))),
        testM("short multiplication")(checkAllLaws(Commutative)(Gen.anyShort.map(Prod(_)))),
        testM("tuple2")(checkAllLaws(Commutative)(anySumInt.zip(anySumInt))),
        testM("tuple3")(
          checkAllLaws(Commutative)(anySumInt.zip(anySumInt).zip(anySumInt).map { case ((x, y), z) => (x, y, z) })
        )
      )
    )
}
