package zio.prelude

import zio.prelude.laws.CommutativeLaws
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
        test("boolean conjuction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(Or(_)))),
        test("byte addition")(checkAllLaws(CommutativeLaws)(Gen.anyByte.map(Sum(_)))),
        test("byte multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyByte.map(Prod(_)))),
        test("char addition")(checkAllLaws(CommutativeLaws)(Gen.anyChar.map(Sum(_)))),
        test("char multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyChar.map(Prod(_)))),
        test("double addition")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Sum(_)))),
        test("double max")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Max(_)))),
        test("double min")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Min(_)))),
        test("double multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(CommutativeLaws)(Gen.anyFiniteDuration)),
        test("either")(checkAllLaws(CommutativeLaws)(Gen.either(anySumInt, anySumInt))),
        test("float addition")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Sum(_)))),
        test("float max")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Max(_)))),
        test("float min")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Min(_)))),
        test("float multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Prod(_)))),
        test("int addition")(checkAllLaws(CommutativeLaws)(anySumInt)),
        test("int multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyInt.map(Prod(_)))),
        test("long addition")(checkAllLaws(CommutativeLaws)(Gen.anyLong.map(Sum(_)))),
        test("long multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyLong.map(Prod(_)))),
        test("map")(checkAllLaws(CommutativeLaws)(Gen.mapOf(anySumInt, anySumInt))),
        test("option")(checkAllLaws(CommutativeLaws)(Gen.option(anySumInt))),
        test("set")(checkAllLaws(CommutativeLaws)(Gen.setOf(anySumInt))),
        test("short addition")(checkAllLaws(CommutativeLaws)(Gen.anyShort.map(Sum(_)))),
        test("short multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyShort.map(Prod(_)))),
        test("tuple2")(checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt))),
        test("tuple3")(
          checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt).zip(anySumInt))
        )
      )
    )
}
