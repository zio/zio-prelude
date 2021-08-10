package zio.prelude

import zio.prelude.newtypes.{And, Max, Min, Or, Prod, Sum}
import zio.test._
import zio.test.laws._
import zio.{Has, Random}

object CommutativeSpec extends DefaultRunnableSpec {

  val anySumInt: Gen[Has[Random], Sum[Int]] = Gen.anyInt.map(Sum(_))

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeSpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(Commutative)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(Commutative)(Gen.boolean.map(Or(_)))),
        test("byte addition")(checkAllLaws(Commutative)(Gen.anyByte.map(Sum(_)))),
        test("byte multiplication")(checkAllLaws(Commutative)(Gen.anyByte.map(Prod(_)))),
        test("char addition")(checkAllLaws(Commutative)(Gen.anyChar.map(Sum(_)))),
        test("char multiplication")(checkAllLaws(Commutative)(Gen.anyChar.map(Prod(_)))),
        test("double addition")(checkAllLaws(Commutative)(Gen.anyDouble.map(Sum(_)))),
        test("double max")(checkAllLaws(Commutative)(Gen.anyDouble.map(Max(_)))),
        test("double min")(checkAllLaws(Commutative)(Gen.anyDouble.map(Min(_)))),
        test("double multiplication")(checkAllLaws(Commutative)(Gen.anyDouble.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(Commutative)(Gen.anyFiniteDuration)),
        test("either")(checkAllLaws(Commutative)(Gen.either(anySumInt, anySumInt))),
        test("float addition")(checkAllLaws(Commutative)(Gen.anyFloat.map(Sum(_)))),
        test("float max")(checkAllLaws(Commutative)(Gen.anyFloat.map(Max(_)))),
        test("float min")(checkAllLaws(Commutative)(Gen.anyFloat.map(Min(_)))),
        test("float multiplication")(checkAllLaws(Commutative)(Gen.anyFloat.map(Prod(_)))),
        test("int addition")(checkAllLaws(Commutative)(anySumInt)),
        test("int multiplication")(checkAllLaws(Commutative)(Gen.anyInt.map(Prod(_)))),
        test("long addition")(checkAllLaws(Commutative)(Gen.anyLong.map(Sum(_)))),
        test("long multiplication")(checkAllLaws(Commutative)(Gen.anyLong.map(Prod(_)))),
        test("map")(checkAllLaws(Commutative)(Gen.mapOf(anySumInt, anySumInt))),
        test("option")(checkAllLaws(Commutative)(Gen.option(anySumInt))),
        test("set")(checkAllLaws(Commutative)(Gen.setOf(anySumInt))),
        test("short addition")(checkAllLaws(Commutative)(Gen.anyShort.map(Sum(_)))),
        test("short multiplication")(checkAllLaws(Commutative)(Gen.anyShort.map(Prod(_)))),
        test("tuple2")(checkAllLaws(Commutative)(anySumInt.zip(anySumInt))),
        test("tuple3")(
          checkAllLaws(Commutative)(anySumInt.zip(anySumInt).zip(anySumInt))
        )
      )
    )
}
