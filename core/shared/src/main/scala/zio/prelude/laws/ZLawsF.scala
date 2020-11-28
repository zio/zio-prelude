package zio.prelude.laws

import zio.URIO
import zio.test.laws.GenF
import zio.test.{ Gen, TestConfig, TestResult, check }

object ZLawsF {

  object Traversable {
    abstract class MapLaw[-CapsF[_[+_]], -Caps[_]](label: String)
        extends zio.test.laws.ZLawsF.Covariant[CapsF, Caps, Any] { self =>
      def apply[F[+_]: CapsF, A: Caps, B: Caps](fa: F[A], f: A => B): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, A: Caps](genF: GenF[R, F], gen: Gen[R, A]): URIO[R, TestResult] =
        check(genF(gen), Gen.function(gen))(apply(_, _).map(_.label(label)))
    }

    abstract class PurityLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]](label: String) { self =>
      def apply[F[+_]: CapsF, G[+_]: CapsG, A: Caps](fa: F[A]): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](genF: GenF[R, F], gen: Gen[R, A]): URIO[R, TestResult] =
        check(genF(gen))(apply(_).map(_.label(label)))
    }

    abstract class FusionLaw[-CapsF[_[+_]], -CapsGH[_[+_]], -Caps[_]](label: String) { self =>
      def apply[F[+_]: CapsF, G[+_]: CapsGH, H[+_]: CapsGH, A: Caps, B: Caps, C: Caps](
        fa: F[A],
        agb: A => G[B],
        bhc: B => H[C]
      ): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, G[+_]: CapsGH, H[+_]: CapsGH, A: Caps, B: Caps, C: Caps](
        genF: GenF[R, F],
        genG: GenF[R, G],
        genH: GenF[R, H],
        gen: Gen[R, A]
      ): URIO[R, TestResult] =
        check(genF(gen), Gen.function(genG(gen)), Gen.function(genH(gen)))(apply(_, _, _).map(_.label(label)))
    }
  }
}
