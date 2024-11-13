package zio.prelude

import zio.Trace
import zio.prelude.laws._
import zio.prelude.data.Optional
import zio.test._
import zio.test.laws._

object IdentityBothSpec extends ZIOBaseSpec {

  val optional: GenF[Any, Optional] =
    new GenF[Any, Optional] {
      def apply[R1, A](gen: Gen[R1, A])(implicit trace: Trace): Gen[R1, Optional[A]] =
        Gen.option(gen)
    }

  def spec: Spec[Environment, Any] =
    suite("IdentityBothSpec")(
      suite("laws")(
        test("either")(checkAllLaws(IdentityBothLaws)(GenF.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityBothLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(IdentityBothLaws)(optional, Gen.int)),
        test("try")(checkAllLaws(IdentityBothLaws)(GenFs.tryScala, Gen.int))
      )
    )
}
