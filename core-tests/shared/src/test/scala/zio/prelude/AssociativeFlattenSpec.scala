package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object AssociativeFlattenSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("AssociativeFlattenSpec")(
      suite("laws")(
        test("map")(checkAllLaws(AssociativeFlattenLaws)(GenFs.map(Gen.int), Gen.int))
      ),
      suite("operators")(
        test("filter") {
          def filter[F[+_]: AssociativeFlatten: Covariant: IdentityBoth: IdentityEither, A](
            fa: F[A]
          )(f: A => Boolean): F[A] =
            fa.filter(f)
          check(Gen.listOf(Gen.int), Gen.function(Gen.boolean)) { (as, f) =>
            val actual   = filter(as)(f)
            val expected = as.filter(f)
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
