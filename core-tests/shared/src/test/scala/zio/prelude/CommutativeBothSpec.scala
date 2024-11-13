package zio.prelude

import zio.Trace
import zio.prelude.laws.CommutativeBothLaws
import zio.prelude.data.Optional
import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends ZIOBaseSpec {

  val optional: GenF[Any, Optional] =
    new GenF[Any, Optional] {
      def apply[R1, A](gen: Gen[R1, A])(implicit trace: Trace): Gen[R1, Optional[A]] =
        Gen.option(gen)
    }

  def spec: Spec[Environment, Any] =
    suite("CommutativeBothSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(CommutativeBothLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("list")(checkAllLaws(CommutativeBothLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(CommutativeBothLaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(CommutativeBothLaws)(optional, Gen.int)),
        test("vector")(checkAllLaws(CommutativeBothLaws)(GenF.vector, Gen.vectorOf(Gen.int)))
      )
    )
}
