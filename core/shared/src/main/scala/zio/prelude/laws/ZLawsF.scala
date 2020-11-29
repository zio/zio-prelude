package zio.prelude.laws

import zio.test.laws.GenF
import zio.test.laws.ZLawsF.Covariant
import zio.test.{ Gen, TestConfig, TestResult, check }
import zio.{ URIO, ZIO }

object ZLawsF {

  abstract class Traversable[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_], -R] { self =>

    def run[R1 <: R with TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
      genF: GenF[R1, F],
      genG: GenF[R1, G],
      gen: Gen[R1, A]
    ): ZIO[R1, Nothing, TestResult]

    def +[CapsF1[x[+_]] <: CapsF[x], CapsG1[x[+_]] <: CapsG[x], Caps1[x] <: Caps[x], R1 <: R](
      that: Traversable[CapsF1, CapsG1, Caps1, R1]
    ): Traversable[CapsF1, CapsG1, Caps1, R1] =
      Traversable.Both(self, that)

    def +[CapsF1[x[+_]] <: CapsF[x], Caps1[x] <: Caps[x], R1 <: R](
      that: Covariant[CapsF1, Caps1, R1]
    ): Traversable[CapsF1, CapsG, Caps1, R1] =
      Traversable.WithCovariant(self, that)
  }

  object Covariant {
    abstract class MapLaw[-CapsF[_[+_]], -Caps[_]](label: String) extends Covariant[CapsF, Caps, Any] { self =>
      def apply[F[+_]: CapsF, A: Caps, B: Caps](fa: F[A], f: A => B): TestResult
      final def run[R <: TestConfig, F[+_]: CapsF, A: Caps](genF: GenF[R, F], gen: Gen[R, A]): URIO[R, TestResult] =
        check(genF(gen), Gen.function(gen))(apply(_, _).map(_.label(label)))
    }
  }

  object Traversable {
    private final case class Both[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_], -R](
      left: Traversable[CapsF, CapsG, Caps, R],
      right: Traversable[CapsF, CapsG, Caps, R]
    ) extends Traversable[CapsF, CapsG, Caps, R] {
      override def run[R1 <: R with TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
        genF: GenF[R1, F],
        genG: GenF[R1, G],
        gen: Gen[R1, A]
      ): ZIO[R1, Nothing, TestResult] =
        left.run(genF, genG, gen).zipWith(right.run(genF, genG, gen))(_ && _)
    }

    private final case class WithCovariant[-CapsF1[_[+_]], -CapsG[_[+_]], -Caps[_], -R](
      left: Traversable[CapsF1, CapsG, Caps, R],
      right: Covariant[CapsF1, Caps, R]
    ) extends Traversable[CapsF1, CapsG, Caps, R] {
      override def run[R1 <: R with TestConfig, F[+_]: CapsF1, G[+_]: CapsG, A: Caps](
        genF: GenF[R1, F],
        genG: GenF[R1, G],
        gen: Gen[R1, A]
      ): ZIO[R1, Nothing, TestResult] =
        left.run(genF, genG, gen).zipWith(right.run(genF, gen))(_ && _)
    }

    abstract class PurityLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]](label: String)
        extends Traversable[CapsF, CapsG, Caps, Any] { self =>
      def apply[F[+_]: CapsF, G[+_]: CapsG, A: Caps](fa: F[A]): TestResult

      final def run[R1 <: TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
        genF: GenF[R1, F],
        genG: GenF[R1, G],
        gen: Gen[R1, A]
      ): ZIO[R1, Nothing, TestResult] =
        check(genF(gen))(apply[F, G, A](_).map(_.label(label)))
    }

    abstract class NaturalityFusionLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]](label: String)
        extends Traversable[CapsF, CapsG, Caps, Any] {
      def apply[F[+_]: CapsF, G[+_]: CapsG, H[+_]: CapsG, A: Caps](
        fa: F[A],
        aga: A => G[A],
        aha: A => H[A]
      ): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
        genF: GenF[R, F],
        genG: GenF[R, G],
        gen: Gen[R, A]
      ): URIO[R, TestResult] =
        check(genF(gen), Gen.function(genG(gen)), Gen.function(genG(gen)))(apply(_, _, _).map(_.label(label)))
    }

    abstract class SequentialFusionLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]](label: String)
        extends Traversable[CapsF, CapsG, Caps, Any] { self =>
      def apply[F[+_]: CapsF, G[+_]: CapsG, H[+_]: CapsG, A, B, C: Caps](
        fa: F[A],
        agb: A => G[B],
        bhc: B => H[C]
      ): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
        genF: GenF[R, F],
        genG: GenF[R, G],
        gen: Gen[R, A]
      ): URIO[R, TestResult] =
        check(genF(gen), Gen.function(genG(gen)), Gen.function(genG(gen)))(apply(_, _, _).map(_.label(label)))
    }

    abstract class ParallelFusionLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]](label: String)
        extends Traversable[CapsF, CapsG, Caps, Any] {
      def apply[F[+_]: CapsF, G[+_]: CapsG, H[+_]: CapsG, A, B: Caps](
        fa: F[A],
        aga: A => G[B],
        aha: A => H[B]
      ): TestResult

      final def run[R <: TestConfig, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
        genF: GenF[R, F],
        genG: GenF[R, G],
        gen: Gen[R, A]
      ): URIO[R, TestResult] =
        check(genF(gen), Gen.function(genG(gen)), Gen.function(genG(gen)))(apply(_, _, _).map(_.label(label)))
    }
  }
}
