package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends ZIOBaseSpec {
  import zio.prelude.Fixtures._

  val genBoolean: Gen[Any, Boolean] =
    Gen.boolean

  val genInt: Gen[Any, Int] =
    Gen.int

  val genList: Gen[Sized, List[Int]] =
    Gen.listOf(genInt)

  def spec: Spec[Environment, Any] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityFlattenLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("either")(checkAllLaws(IdentityFlattenLaws)(GenFs.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityFlattenLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityFlattenLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(IdentityFlattenLaws)(optionalGenF, Gen.int)),
        test("vector")(checkAllLaws(IdentityFlattenLaws)(GenF.vector, Gen.int))
      ),
      suite("combinators")(
        test("when") {
          check(genList, genBoolean) { (as, b) =>
            val actual   = as.when(b)
            val expected = if (b) as.map(Some(_)) else List(None)
            assert(actual)(equalTo(expected))
          }
        },
        test("unless") {
          check(genList, genBoolean) { (as, b) =>
            val actual   = as.unless(b)
            val expected = if (b) List(None) else as.map(Some(_))
            assert(actual)(equalTo(expected))
          }
        }
      )
    )

}
