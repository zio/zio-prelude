package zio.prelude

import zio.prelude.laws.CommutativeLaws
import zio.prelude.newtypes.{And, Max, Min, Or, OrF, Prod, Sum}
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
        testM("boolean conjuction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(And(_)))),
        testM("boolean disjunction")(checkAllLaws(CommutativeLaws)(Gen.boolean.map(Or(_)))),
        testM("byte addition")(checkAllLaws(CommutativeLaws)(Gen.anyByte.map(Sum(_)))),
        testM("byte multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyByte.map(Prod(_)))),
        testM("char addition")(checkAllLaws(CommutativeLaws)(Gen.anyChar.map(Sum(_)))),
        testM("char multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyChar.map(Prod(_)))),
        testM("double addition")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Sum(_)))),
        testM("double max")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Max(_)))),
        testM("double min")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Min(_)))),
        testM("double multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyDouble.map(Prod(_)))),
        testM("duration ZIO")(checkAllLaws(CommutativeLaws)(Gen.anyFiniteDuration)),
        testM("either")(checkAllLaws(CommutativeLaws)(Gen.either(anySumInt, anySumInt))),
        testM("float addition")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Sum(_)))),
        testM("float max")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Max(_)))),
        testM("float min")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Min(_)))),
        testM("float multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyFloat.map(Prod(_)))),
        testM("int addition")(checkAllLaws(CommutativeLaws)(anySumInt)),
        testM("int multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyInt.map(Prod(_)))),
        testM("long addition")(checkAllLaws(CommutativeLaws)(Gen.anyLong.map(Sum(_)))),
        testM("long multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyLong.map(Prod(_)))),
        testM("map")(checkAllLaws(CommutativeLaws)(Gen.mapOf(anySumInt, anySumInt))),
        testM("option")(checkAllLaws(CommutativeLaws)(Gen.option(anySumInt))),
        testM("set")(checkAllLaws(CommutativeLaws)(Gen.setOf(anySumInt).map(OrF(_)))),
        testM("short addition")(checkAllLaws(CommutativeLaws)(Gen.anyShort.map(Sum(_)))),
        testM("short multiplication")(checkAllLaws(CommutativeLaws)(Gen.anyShort.map(Prod(_)))),
        testM("tuple2")(checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt))),
        testM("tuple3")(
          checkAllLaws(CommutativeLaws)(anySumInt.zip(anySumInt).zip(anySumInt).map { case ((x, y), z) => (x, y, z) })
        )
      )
    )
}
