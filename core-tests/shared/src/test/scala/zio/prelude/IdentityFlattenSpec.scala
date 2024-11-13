package zio.prelude

import zio.Trace
import zio.prelude.laws._
import zio.prelude.data.Optional
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends ZIOBaseSpec {

  val optional: GenF[Any, Optional] =
    new GenF[Any, Optional] {
      def apply[R1, A](gen: Gen[R1, A])(implicit trace: Trace): Gen[R1, Optional[A]] =
        Gen.option(gen)
    }

  def spec: Spec[Environment, Any] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityFlattenLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("either")(checkAllLaws(IdentityFlattenLaws)(GenFs.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityFlattenLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityFlattenLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(IdentityFlattenLaws)(optional, Gen.int)),
        test("vector")(checkAllLaws(IdentityFlattenLaws)(GenF.vector, Gen.int))
      )
    )

}
