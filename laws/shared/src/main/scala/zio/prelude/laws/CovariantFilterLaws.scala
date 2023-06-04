package zio.prelude
package laws

import zio.prelude.Equal
import zio.prelude.coherent.CovariantFilterDeriveEqual
import zio.test.TestResult
import zio.test.laws._
object CovariantFilterLaws extends LawfulF.Covariant[CovariantFilterDeriveEqual, Equal] {
  lazy val compositionLaw: LawsF.Covariant[CovariantFilterDeriveEqual, Equal] =
    new LawsF.Covariant.ComposeLaw[CovariantFilterDeriveEqual, Equal]("compositionLaw") {
      def apply[F[+_]: CovariantFilterDeriveEqual, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        f: A => B,
        g: B => C
      ): TestResult = {
        val F: A => Option[B] = f.andThen(Option(_))
        val G: B => Option[C] = g.andThen(Option(_))

        fa.mapFilter(F).mapFilter(G) <-> fa.mapFilter(F(_).flatMap(G))
      }
    }

  lazy val identityLaw: LawsF.Covariant[CovariantFilterDeriveEqual, Equal] =
    new LawsF.Covariant.Law1[CovariantFilterDeriveEqual, Equal]("identityLaw") {
      def apply[F[+_]: CovariantFilterDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.mapFilter(a => Some(identity(a))) <-> fa
    }

  lazy val consistencyLaw: LawsF.Covariant[CovariantFilterDeriveEqual, Equal] =
    new LawsF.Covariant.Law1[CovariantFilterDeriveEqual, Equal]("consistencyLaw") {
      def apply[F[+_]: CovariantFilterDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.mapFilter(Some(_)) <-> CovariantFilterDeriveEqual.derive[F].covariant.map[A, A](identity)(fa)
    }

  lazy val laws: LawsF.Covariant[CovariantFilterDeriveEqual, Equal] =
    identityLaw + compositionLaw + consistencyLaw
}
