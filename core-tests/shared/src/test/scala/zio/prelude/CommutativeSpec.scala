package zio.prelude

import zio.prelude.laws.CommutativeLaws
import zio.prelude.newtypes.{And, Max, Min, Or, OrF, Prod, Sum}
import zio.test._
import zio.test.laws._

object CommutativeSpec extends ZIOSpecDefault {

  val anySumInt: Gen[Any, Sum[Int]] = Gen.int.map(Sum(_))

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("CommutativeSpec")(
      suite("laws")(
        test("boolean conjuction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(And(_)))),
        test("boolean disjunction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(Or(_)))),
        test("byte addition")(checkAllLaws(CommutativeLaws)(Gen.byte.map(Sum(_)))),
        test("byte multiplication")(checkAllLaws(CommutativeLaws)(Gen.byte.map(Prod(_)))),
        test("char addition")(checkAllLaws(CommutativeLaws)(Gen.char.map(Sum(_)))),
        test("char multiplication")(checkAllLaws(CommutativeLaws)(Gen.char.map(Prod(_)))),
        test("double addition")(checkAllLaws(CommutativeLaws)(Gen.double.map(Sum(_)))),
        test("double max")(checkAllLaws(CommutativeLaws)(Gen.double.map(Max(_)))),
        test("double min")(checkAllLaws(CommutativeLaws)(Gen.double.map(Min(_)))),
        test("double multiplication")(checkAllLaws(CommutativeLaws)(Gen.double.map(Prod(_)))),
        test("duration ZIO")(checkAllLaws(CommutativeLaws)(Gen.finiteDuration)),
        test("either")(checkAllLaws(CommutativeLaws)(Gen.either(anySumInt, anySumInt))),
        test("float addition")(checkAllLaws(CommutativeLaws)(Gen.float.map(Sum(_)))),
        test("float max")(checkAllLaws(CommutativeLaws)(Gen.float.map(Max(_)))),
        test("float min")(checkAllLaws(CommutativeLaws)(Gen.float.map(Min(_)))),
        test("float multiplication")(checkAllLaws(CommutativeLaws)(Gen.float.map(Prod(_)))),
        test("int addition")(checkAllLaws(CommutativeLaws)(anySumInt)),
        test("int multiplication")(checkAllLaws(CommutativeLaws)(Gen.int.map(Prod(_)))),
        test("long addition")(checkAllLaws(CommutativeLaws)(Gen.long.map(Sum(_)))),
        test("long multiplication")(checkAllLaws(CommutativeLaws)(Gen.long.map(Prod(_)))),
        test("map")(checkAllLaws(CommutativeLaws)(Gen.mapOf(anySumInt, anySumInt))),
        test("option")(checkAllLaws(CommutativeLaws)(Gen.option(anySumInt))),
        test("set")(checkAllLaws(CommutativeLaws)(Gen.setOf(anySumInt).map(OrF(_)))),
        test("short addition")(checkAllLaws(CommutativeLaws)(Gen.short.map(Sum(_)))),
        test("short multiplication")(checkAllLaws(CommutativeLaws)(Gen.short.map(Prod(_)))),
        test("tuple2")(checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt))),
        test("tuple3")(
          checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt).zip(anySumInt))
        )
      )
    )
}
